#!/usr/bin/python3
import sys
import argparse
import subprocess
import shutil
import os

parser = argparse.ArgumentParser(description='Compile tony file')
parser.add_argument('-O', dest='O_flag', action='store_true',
                     default=False,
                    help="Allow optimizations")

parser.add_argument('-f', dest='f_flag', action='store_true',
                     default=False,
                    help="Input must be given on stdin and the final code will be printed in stdout")
parser.add_argument('-i', dest='i_flag', action='store_true',
                     default=False,
                    help="Input must be given on stdin and the intermediate code will be printed in stdout")
parser.add_argument('file',type=str, action='store',
                     default=None, nargs="?",
                    help="Input file")
args = parser.parse_args()

if not args.f_flag and not args.i_flag and args.file==None:
    print("If no -i or -f options are given an input file must be given")
    exit(1) 

if args.file==None:
    input = ""
    for line in sys.stdin:
        input+=line
    # print(input)
    f = open("__tmp__.txt","w")
    f.write(input)
    f.close()
    file = "__tmp__.txt"
else:
    file = args.file
    if args.file.endswith(".tony"):
        tmp = args.file[:len(args.file)-len(".tony")]
        i_file = tmp+".imm"
        f_file = tmp+".asm"
        ex_file = tmp+".out"
    else:
        i_file = args.file+".imm"
        f_file = args.file+".asm"
        ex_file = args.file+".out"

if args.O_flag:
    opt = "1"
else:
    opt = "0"

p = subprocess.Popen("./tony "+opt+" < "+file+" > a.ll", stdout=subprocess.PIPE,stderr=subprocess.PIPE, shell=True)
(output, err) = p.communicate()
p_status = p.wait()
if p_status!=0:
    print(err.decode("utf-8"))
    exit(1)

if args.i_flag:
    p=subprocess.Popen("cat a.ll", shell=True)
    p.wait()

p = subprocess.Popen("llc a.ll -o a.s",shell=True)
p.wait()

if args.f_flag:
    p=subprocess.Popen("cat a.s", shell=True)
    p.wait()

p = subprocess.Popen("clang -o a.out a.s lib.a -lgc",shell=True)
p.wait()

if not args.i_flag and not args.f_flag:
    shutil.move("a.ll", i_file)
    shutil.move("a.s", f_file)
    shutil.move("a.out", ex_file)
else:
    os.remove("a.ll")
    os.remove("a.s")
    os.remove("__tmp__.txt")

