package com.nirvana.holder

import java.util.Calendar

import com.nirvana.activity.Activity
import com.nirvana.actor.{BanditMonster, BanditNpc, Player}
import com.nirvana.core.{Constants, Initialize, MapBean}
import com.nirvana.cross.CrossHelper
import com.nirvana.exception.BusinessException
import com.nirvana.message.MessageHolder
import com.nirvana.net.{GameServerHolder, Message, PlayerChannel}
import com.nirvana.partnership.PlayerTmpInfo
import com.nirvana.scene.{Scene, SceneHolder}
import com.nirvana.service.PlayerService
import com.nirvana.team.TeamHolder
import com.nirvana.thread.Executor
import com.nirvana.util.{MessageI18n, Tool}
import com.nirvana.xml._
import grizzled.slf4j.Logging
import org.apache.commons.lang.time.DateUtils

import scala.util.Random

/**
 * 讨伐贼寇Holder
 */
object BanditPunishHolder extends Initialize with Logging {

  final val REQ_DISTANCE = 3 * 1000
  var sceneNpcMap = Map.empty[Scene, List[BanditNpc]]
  final val debug = false

  //下次自动刷新时间
  var nextAutoRefresh = 0L

  /**
   * 刷新贼寇NPC
   * @param treasureMapQua 藏宝图品质
   * @param player 挖出贼寇玩家
   */
  def refreshBanditNpc(player : Player, treasureMapQua : Int): Unit ={
    if(debug) logger.info(s"宝图品质:$treasureMapQua")
    val scene = player.movement.scene
    if(scene == null){
      warn("refresh bandit npc failed, player scene is null!!!")
      return
    }

    val banditCount = BanditConfig.getRandomBanditNpcCount
    if(debug) logger.info(s"随机即将要刷出:$banditCount 个贼寇npc")


    //本地图所有贼寇刷新点信息
    val bornInfo = SceneConfig.getNpcBornByScene(scene.sceneDef.id,  Constants.NPC_BORN_TYPE_BANDIT).headOption.getOrElse(
      throw new RuntimeException(s"scene id:${scene.sceneDef.id} get bandit born not found"))

    for (i <- 1 to banditCount){
      //随机出贼寇星级
      val banditType = treasureMapQua match {
        case Constants.TREASURE_COMMON =>
          BanditConfig.commonMapBanditTypeChooser.randomGet
        case Constants.TREASURE_SENIOR =>
          BanditConfig.advanceMapBanditTypeChooser.randomGet
        case Constants.TREASURE_CROSS =>
          BanditConfig.crossMapBanditTypeChooser.randomGet
      }
      refreshBanditNpcEach(i, banditCount, scene, banditType, bornInfo.npcBornPoints)
    }

    //通知
    treasureMapQua match {
      case Constants.TREASURE_SENIOR =>
        val href = MessageHolder.getJumpPanelHref(61, MessageI18n.getString("Bandit_9"), params = List("5"))
        val prod = ProductConfig.getSystemProduct(Constants.TREASURE_SENIOR_ITEM_ID)
        val prodHref = MessageHolder.getProductHref(prod.id)
        MessageHolder.sendSysWoldToAll(MessageI18n.getString("Bandit_16", player.name, scene.sceneDef.name, s"<${prod.name}>"), prodHref :: href)
      case Constants.TREASURE_CROSS =>
        val href = MessageHolder.getJumpPanelHref(103, MessageI18n.getString("Bandit_25"), params = List("5"))
        MessageHolder.sendSysWoldToAll(MessageI18n.getString("Bandit_24", player.name, scene.sceneDef.name,BanditConfig.crossBanditNpcRefreshCount), href)
      case _ =>
        val href = MessageHolder.getJumpPanelHref(61, MessageI18n.getString("Bandit_9"), params = List("5"))
        MessageHolder.sendSysWoldToAll(MessageI18n.getString("Bandit_1", player.name, scene.sceneDef.name), href)
    }

  }

  /**
    * 刷新跨服贼寇NPC
    * @param treasureMapQua 藏宝图品质
    * @param player 挖出贼寇玩家
    */
  def refreshCrossBanditNpc(player : Player, treasureMapQua : Int): Unit ={
    val scene = player.movement.scene
    if(scene == null){
      warn("refresh bandit npc failed, player scene is null!!!")
      return
    }
    val banditCount = BanditConfig.crossBanditNpcRefreshCount
    if(debug) logger.info(s"随机即将要刷出:$banditCount 个贼寇npc")
    //本地图所有马贼刷新点信息
    val bornInfo = SceneConfig.getNpcBornByScene(scene.sceneDef.id, Constants.NPC_BORN_TYPE_CROSS_TREASURE).headOption.getOrElse(
      throw new RuntimeException(s"scene id:${scene.sceneDef.id} get bandit born not found"))

    for (i <- 1 to banditCount){
      //随机出贼寇星级
      val banditType = treasureMapQua match {
        case Constants.TREASURE_CROSS =>
          BanditConfig.crossMapBanditTypeChooser.randomGet
      }
      refreshCrossBanditNpcEach(i, banditCount, banditType, bornInfo.npcBornPoints,player)
    }
    noticeCrossBanditNumChange
    //通知
    val href = MessageHolder.getJumpPanelHref(103, MessageI18n.getString("Bandit_25"), params = List("5"))
    MessageHolder.sendSysWoldToAll(MessageI18n.getString("Bandit_24", player.name, scene.sceneDef.name,BanditConfig.crossBanditNpcRefreshCount), href)
    //发送到游戏服
    val param = List(player.name, scene.sceneDef.name)
    val panelHref = MessageHolder.getJumpPanelHref(103, MessageI18n.getString("OutOfSky_6"))
    CrossHelper.sendToGameHorseRaceLampBanner(14001, param,panelHref)

  }

  /**
    * 刷新贼寇
    * @param idx
    * @param total
    * @param scene
    * @param banditType
    * @param bornInfo
    */
  private def refreshBanditNpcEach(idx : Int, total : Int, scene : Scene, banditType : Int, bornInfo : List[(Int, Int, Int)]): Unit ={

    if(debug) logger.info(s"开始处理第${idx} 个贼寇, 随机出贼寇类型:$banditType")
    val banditDef = BanditConfig.getRandomBanditByType(banditType)

    //看下这个星级的贼寇在此地图最大存在数量
    val max =
      if (!GameServerHolder.isCrossServer) BanditConfig.qualityAliveCount.getOrElse(banditType, 0)
      else BanditConfig.crossQualityAliveCount


    //如果这种类型贼寇已经满了, 移除最旧的
    val sameTypeList = sceneNpcMap.getOrElse(scene, List.empty[BanditNpc]).filter(_.banditDef.`type` == banditType)
    //    val banditCount = 3
    if(debug) logger.info(s"${scene.sceneDef.name}最大可同时存在type=${banditType}的贼贼寇数:${max}个, 当前存在此类型贼寇npc数:${sameTypeList.size}")
    if(sameTypeList.size >= max){
      if(debug) logger.info(s"超出最大可存在贼寇npc数:${max} 个, 即将移除旧的")
      val oldest = sameTypeList.minBy(_.startTime)
      if(debug) logger.info(s"移除最早刷的贼寇npc:${(oldest.movement.x, oldest.movement.z,oldest.movement.angle)}")
      removeBanditNpc(oldest)
    }

    //本地图存在的贼寇npc列表
    val existNpcLs = sceneNpcMap.getOrElse(scene, List.empty)
    //获取已有的贼寇刷新点
    val existPoints = existNpcLs.map(m => (m.movement.x.toInt, m.movement.z.toInt, m.movement.angle.toInt))
    //过滤掉已经有刷npc的点
    val newPoints = bornInfo.toSet -- existPoints

    //如果新的点不够, 用旧的点填充
    val refillNewPoint = if(newPoints.isEmpty) {
      warn(s"WARNING : scene id:${scene.sceneDef.id} born point length ${newPoints.size} < 0")
      newPoints ++ Random.shuffle(bornInfo).slice(0,  total - newPoints.size).toSet
    } else newPoints

    val bornPoint = Random.shuffle(refillNewPoint.toList).head
    if(debug) logger.info(s"即将刷新不重复的点:${bornPoint}")

    val banditNpc = scene.monsterBuilder.buildBanditNpc(banditDef, bornPoint._1, bornPoint._2, bornPoint._3)
    addNpcToMap(banditNpc)
    if(debug) logger.info(s"刷出贼寇npc:${(banditNpc.movement.x, banditNpc.movement.z,banditNpc.movement.angle)}")
  }

  /**
    * 刷新跨服马贼
    * @param idx
    * @param total
    * @param banditType
    * @param bornInfo
    * @param player
    */
  private def refreshCrossBanditNpcEach(idx : Int, total : Int,  banditType : Int, bornInfo : List[(Int, Int, Int)],player: Player): Unit ={
    if(debug) logger.info(s"开始处理第${idx} 个马贼, 马贼类型:$banditType")
    val banditDef = BanditConfig.getRandomBanditByType(banditType)
    //看下这个星级的贼寇在此地图最大存在数量
    val max = BanditConfig.crossQualityAliveCount
    //如果场景类内马贼已经满了, 移除最旧的
    val sameTypeList = sceneNpcMap.getOrElse(player.movement.scene, List.empty[BanditNpc]).filter(_.banditDef.`type` == banditType)
    //    val banditCount = 3
    if(debug) logger.info(s"${player.movement.scene.sceneDef.name}最大可同时存在type=${banditType}的马贼数:${max}个, 当前存在马贼npc数:${sameTypeList.size}")
    if(sameTypeList.size >= max){
      if(debug) logger.info(s"超出最大可存在马贼npc数:${max} 个, 即将移除旧的")
      val oldest = sameTypeList.minBy(_.startTime)
      if(debug) logger.info(s"移除最早刷的马贼npc:${(oldest.movement.x, oldest.movement.z,oldest.movement.angle)}")
      removeBanditNpc(oldest)
    }

    //本地图存在的马贼npc列表
    val existNpcLs = sceneNpcMap.getOrElse(player.movement.scene, List.empty)
    //获取已有的贼寇刷新点
    val existPoints = existNpcLs.map(m => (m.movement.x.toInt, m.movement.z.toInt, m.movement.angle.toInt))
    //过滤掉已经有刷npc的点
    val newPoints = bornInfo.toSet -- existPoints

    //如果新的点不够, 用旧的点填充
    val refillNewPoint = if(newPoints.isEmpty) {
      warn(s"WARNING : scene id:${player.movement.scene.sceneDef.id} born point length ${newPoints.size} < 0")
      newPoints ++ Random.shuffle(bornInfo).slice(0,  total - newPoints.size).toSet
    } else newPoints

    val bornPoint = Random.shuffle(refillNewPoint.toList).head
    if(debug) logger.info(s"即将刷新不重复的点:${bornPoint}")

    val banditNpc = player.movement.scene.monsterBuilder.buildBanditNpc(banditDef, bornPoint._1, bornPoint._2, bornPoint._3)
    if (banditDef.`type` == Constants.BANDIT_CROSS) {
      banditNpc.owner = PlayerTmpInfo.fromPlayer(player)
    }
    addNpcToMap(banditNpc)
    if(debug) logger.info(s"刷出马贼npc:${(banditNpc.movement.x, banditNpc.movement.z,banditNpc.movement.angle)}")
  }

  // 通知场景内所有玩家当前马贼总数量变更
  def noticeCrossBanditNumChange(): Unit ={
    // 通知跨服蓬莱岛内所有玩家当前马贼总数量
    val curCnt = getCrossBanditNum()
    SceneHolder.getScene(Constants.OUT_OF_SKY_SCENEID + "_" + 1).players.foreach(p =>{
      PlayerChannel.send(Message(0x3204, MapBean("curCnt" -> curCnt)), p.id)
    })
  }

  //蓬莱岛已存在马贼数量
  def getCrossBanditNum():Int = {
    sceneNpcMap.getOrElse(SceneHolder.getScene(Constants.OUT_OF_SKY_SCENEID + "_" + 1), List.empty[BanditNpc]).count(_.banditDef.`type` == Constants.BANDIT_CROSS)
  }


  //获取玩家当前场景自己召唤马贼NPC坐标
  def getCrossBanditXYZByPlayer(player: Player): List[MapBean]={
    var bandits = List.empty[MapBean]
    sceneNpcMap.getOrElse(player.movement.scene, List.empty[BanditNpc]).filter(_.banditDef.`type` == Constants.BANDIT_CROSS).filter(_.owner.playerId == player.id).foreach(bandit =>{
      bandits ::= MapBean("mid" -> bandit.id,"x" -> bandit.movement.x.toInt,"y" -> bandit.movement.y.toInt,"z" -> bandit.movement.z.toInt)
    })
    bandits
  }

  /**
   * 移除贼寇NPC
   * @param npc
   */
  def removeBanditNpc(npc : BanditNpc): Unit ={
    npc.movement.scene.monsterBuilder.removeMonster(npc)
    removeNpcFromMap(npc)
  }

  def refreshBanditMonster(npc : BanditNpc): Unit ={
    npc.movement.scene.monsterBuilder.buildBanditMonster(npc)
  }

  def refreshCrossBanditMonster(npc : BanditNpc): Unit ={
    npc.movement.scene.monsterBuilder.buildCrossBanditMonster(npc)
  }


  /**
   * 通过npc召唤贼寇怪
   * @param player
   * @param npc
   */
  def summonBandit(player : Player, npc : BanditNpc): Unit ={
    //检查条件
//    val playerNeed = 1 //方便调试
    val playerNeed = npc.banditDef.player_need
    if(npc.banditDef.`type` == Constants.BANDIT_CROSS || player.inTeam()) {
      val team = TeamHolder.teamPlayers.getOrElse(player.id, throw new BusinessException(MessageI18n.getString("Bandit_2", playerNeed)))
      if (team.leadId != player.id) throw new BusinessException(MessageI18n.getString("Bandit_3"))
      if (team.teamMembers.size < playerNeed) throw new BusinessException(MessageI18n.getString("Bandit_2", playerNeed))
      if (TimesHolder.getBanditSummonCount(player.id) >= BanditConfig.maxSummonTimes && npc.banditDef.`type` != Constants.BANDIT_CROSS) throw new BusinessException(MessageI18n.getString("Bandit_14"))

      val teamPlayers = team.teamMembers.keys.map(pid => PlayerHolder.getPlayer(pid))

      if (teamPlayers.filterNot(_ == null).forall(p => {
        !canGetReward(npc.banditDef, p) && npc.banditDef.`type` != Constants.BANDIT_CROSS
      })) throw new BusinessException(MessageI18n.getString("Bandit_15"))

      if (teamPlayers.exists(p => {
        p == null || p.movement.scene != npc.movement.scene ||
          !Tool.inDistance(npc.movement.x, npc.movement.y, npc.movement.z, p.movement.x, p.movement.y, p.movement.z, REQ_DISTANCE)
      })) throw new BusinessException(MessageI18n.getString("Bandit_4", npc.banditDef.monster_name))
    } else if(!canGetReward(npc.banditDef, player)) {
      throw new BusinessException(MessageI18n.getString("Bandit_36"))
    }
    removeBanditNpc(npc)
    if (npc.owner.nonEmpty) refreshCrossBanditMonster(npc)
    else refreshBanditMonster(npc)
    TimesHolder.incBanditSummonCount(player.id)
  }




  private def addNpcToMap(npc : BanditNpc): Unit ={
    this.synchronized({
      val scene = npc.movement.scene
      val lsOpt = sceneNpcMap.get(scene)
      val ls = lsOpt match {
        case Some(ls) => (ls :+ npc)
        case None => List(npc)
      }
      sceneNpcMap += scene -> ls
    })
  }


  private def removeNpcFromMap(npc : BanditNpc): Unit ={
    val scene = npc.movement.scene
    val lsOpt = sceneNpcMap.get(scene)
    val ls = lsOpt match {
      case Some(ls) => ls.filterNot(_ == npc)
      case None => return
    }
    sceneNpcMap += scene -> ls
  }

//  def onEvent(eventCode: Int, args: List[Any]): Unit = {
//    eventCode match {
//      //当发生死亡事件时
//      case Constants.INSTANCE_EVENT_DIE =>
//        //怪物死亡
//        if (args(0).isInstanceOf[BanditMonster] && (args(1).isInstanceOf[Player] || args(1).isInstanceOf[Pet])) {
//          val player = args(1) match {
//            case p: Player => p.asInstanceOf[Player]
//            case pet: Pet => pet.asInstanceOf[Pet].owner
//            case _ => return
//          }
//          onBossKill(args(0).asInstanceOf[BanditMonster], player)
//        }
//
//      case _ =>
//    }
//  }


  def init(): Unit ={
    nextAutoRefresh = {
      val cal = Calendar.getInstance()
      cal.clear(Calendar.MINUTE)
      cal.clear(Calendar.SECOND)
      cal.clear(Calendar.MILLISECOND)
      cal.add(Calendar.MINUTE, BanditConfig.autoRefreshRate)
      cal.getTime.getTime
    }
  }

  def tick(index : Int): Unit ={
    if(System.currentTimeMillis() >= nextAutoRefresh && nextAutoRefresh != 0L){
      nextAutoRefresh = nextAutoRefresh + BanditConfig.autoRefreshRate * DateUtils.MILLIS_PER_MINUTE
      if(BanditConfig.activity.inServerOpenDay()){
        Tool.exeWithTry(autoRefresh)
      }
    }
  }


  def autoRefresh(): Unit ={
    if(debug) logger.info("hourly refresh bandit!")
    //对服务器等级以下可刷贼寇的地图
    val maxLevel = ConstantConfig.getServerMaxLevel
    val bornList = SceneConfig.getNpcBornByType(Constants.NPC_BORN_TYPE_BANDIT).filter(born => {
      val sceneDef = SceneConfig.getSceneDef(born.map_id)
      sceneDef != null && sceneDef.level <= maxLevel
    })
    val goToFightHref = MessageHolder.getJumpPanelHref(61, MessageI18n.getString("Bandit_9"), params = List("5"))
    bornList.groupBy(_.map_id).foreach{
      case (sysSceneId, ls) =>
        val bornPoints = ls.map(_.npcBornPoints).flatten
        val sceneId = SceneHolder.buildSceneId(sysSceneId)
        val scene = SceneHolder.getScene(sceneId)
        val currentNpcs = sceneNpcMap.getOrElse(scene, List.empty)
        if(currentNpcs.size < BanditConfig.autoRefreshCheck){
          val refreshCount = BanditConfig.getRandomBanditNpcCount
          for (i <- 1 to refreshCount){
            //随机出贼寇星级
            val banditType = BanditConfig.commonMapBanditTypeChooser.randomGet
            refreshBanditNpcEach(i, refreshCount, scene, banditType, bornPoints)
          }
          MessageHolder.sendSysWoldToAll(MessageI18n.getString("Bandit_17", scene.sceneDef.name), goToFightHref)
        }
    }
  }

  def onBossKill(monster : BanditMonster, player : Player) : Unit = {
    val teamPlayer = TeamHolder.getTeamPlayer(player)
    teamPlayer.foreach(p => {
      if(canGetReward(monster.banditDef, player)){

      }
    })
  }

  /**
    * 是否可以获得奖励
    * @param banditDef
    * @param player  是否奖励的对象
    * @param ownerPid  生成召唤怪的玩家id
    * @return
    */
  def canGetReward(banditDef : BanditDef, player : Player, ownerPid : String = "") = {
    banditDef.`type` match {
      case Constants.BANDIT_LEADER => TimesHolder.getBanditLeaderCount(player.id) < BanditConfig.banditLeaderRewardCount
      case Constants.BANDIT_CROSS => TimesHolder.getBanditCrossCount(player.id) < BanditConfig.banditCrossRewardCount || player.id == ownerPid
      case _ => TimesHolder.getBanditCount(player.id) < BanditConfig.banditRewardCount
    }
  }

  def incRewardCount(monster : BanditMonster, player : Player) = {
    monster.banditDef.`type` match {
      case Constants.BANDIT_LEADER => TimesHolder.incBanditLeaderCount(player.id)
      case Constants.BANDIT_CROSS => {
        TimesHolder.incBanditCrossCount(player.id)
        Activity.addActivity(player, Constants.ACTIVITY_ID_CROSS_BANDIT)
      }
      case _ =>
        TimesHolder.incBanditCount(player.id)
        Activity.addActivity(player, Constants.ACTIVITY_ID_BANDIT)
    }
  }

  /**
   * 获取可传送的讨伐贼寇地图id
   * @param player
   * @return
   */
  def getToMapId(player : Player): String ={
    val minLevel = TeamHolder.teamPlayers.get(player.id) match {
      case None => player.level
      case Some(team) => team.teamMembers.values.minBy(_.level).level
    }

    val list = SceneConfig.getNpcBornByLevel(minLevel, Constants.NPC_BORN_TYPE_BANDIT)
    if(list == null || list.isEmpty) throw new BusinessException(MessageI18n.getString("Bandit_8"))
    val index = Random.nextInt(list.size)
    val born = list(index)
    born.map_id
  }
}


