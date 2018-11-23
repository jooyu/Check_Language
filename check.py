# 第一版，检测代码中的中文，并替换
# /usr/bin/env python
# coding:utf-8

import os
import re
import codecs
import importlib, sys

importlib.reload(sys)

zhPattern = re.compile(u'[\u4e00-\u9fa5]+')  # 用于匹配有中文的行
file_types = ['.pyc', '.jpg', '.png', '.xls,', '.svn-base']  # 默认过滤掉的二进制文件的扩展名
choice_tpye_list = []  # 输入要查找的文件格式
save_path = ''  # 结果保存位置


def start():
    # '''
    # argv[1]:要查询的文件或目录的绝对路径
    # argv[2]:要查询的文件的格式,输入*或者不输入将按默认设置查
    # argv[3]:查询结果的保存路径，绝对路径
    # :return:
    # '''
    global choice_tpye_list, save_path
    try:
        paths = sys.argv[1]
        print('paths', paths)
    except Exception:
        paths = ''
    try:
        choices = sys.argv[2]
        print('choices', choices)
        if choices == '*':
            choices = ''
    except Exception:
        choices = ''
    try:
        save_path = sys.argv[3]
        print('save_path', save_path)
    except Exception:
        save_path = os.path.join(os.getcwd(), 'result.txt')
    choice_types = choices.split(',')
    if len(choice_types) == 1 and '' in choice_types:  # choice_types可能是[''],其布尔值为True
        pass
    else:
        choice_tpye_list = choice_types
    path_list = paths.split(',')
    for path in path_list:
        checkDIR(path)


def checkDIR(path):
    # '''检查是不是文件，是文件处理，不是则向下查找文件'''
    if os.path.isfile(path):
        a, b = os.path.splitext(path)  # 去除扩展名
        if choice_tpye_list:
            if b in choice_tpye_list:  # 检查当前文件扩展名是不是指定查询的扩展名文件
                checkZh(path)
        else:
            if b not in file_types:
                checkZh(path)

    elif os.path.isdir(path):
        file_list = os.listdir(path)
        path_list = map(lambda x: os.path.join(path, x), file_list)  # 转为绝对路径
        for item in path_list:
            checkDIR(item)
    else:
        print(u'---输入错误---')


def checkZh(file):
    # '''查找文件中的中文位置'''
    num = 1
    all_lis = []
    lis = []
    with open(file, 'r', encoding='utf-8') as f:
        line = f.readline()
        while line:
            try:
                line = bytes.decode(bytes(line, 'utf-8'), 'utf-8')
                # line = line.decode('utf-8')
            except Exception as e:
                print(e, '----', file, '---', num, '---', line, u'---文件可能是个二进制文件')

            content_lis = line.split('#')
            match = zhPattern.search(content_lis[0])
            if match:
                lis = [file, num, line]
                all_lis.append(lis)
            line = f.readline()
            num += 1

    with codecs.open(save_path, 'a', 'utf-8') as f:
        f.write('文件%s的查找结果：\n' % file)
        if all_lis:
            for itme in all_lis:
                f.write('    %s   第%s行   %s\n' % (itme[0], itme[1], itme[2]))
        else:
            f.write('    无相关结果\n')


#修改文件名字


if __name__ == '__main__':
    start()
    print(u'***查找结果将在：%s显示***' % save_path)
