# Python4Delphi 安装使用注意
## 摘要
想要在 Delphi 程序里面使用 Python，在 Delphi 里面安装 Python4Delphi 这个控件，编译的 WINDOWS 程序可以调用 WINDOWS 里面安装的 Python。然后，WINDOWS 里面，Python 的安装需要注意。

## 注意事项
本文提到的 WINDOWS 是指 WIN 10.

如果你在 WINDOWS 的命令行（cmd.exe）或者 Power Shell 窗口里面，输入 Python 命令，它会弹出 Windows App Store 让你安装 Python。

假设你在 App Store 里面，选择了某个 Python 的版本，点击安装，安装成功后，可以在命令行窗口里面或者 Power Shell 窗口里面，输入 python 进入 python 命令行状态，在此状态下输入 python 程序代码，可以执行并看到结果。

但是，这样安装的 Python，它的安装路径分为两个部分。

1. 在当前用户的 appdata 目录底下：
~~~
C:\Users\MyUserName\AppData\Local\Microsoft\WindowsApps\PythonSoftwareFoundation.Python.3.10_qbz5n2kfra8p0;
~~~

2. 在 C:\Program Files\WindowsApps 目录底下。这个目录，当前用户是无法访问的。但相应的 DLL 就在这个目录底下。

在上述 1 的目录底下，有 python.exe 但没有对应的 DLL。

而 Python4Delphi 需要调用 python310.dll（具体文件名看对应的版本）。在上述安装情况下，即便把  C:\Program Files\WindowsApp\xXXXX 这个路径加入系统 Path 环境变量，或者直接写入 Python4Delphi 的 DLL Path 属性，运行时都会报找不到 DLL 的错误。

## 正确的安装
去 Python 官网下载安装程序，在 Win 10 上安装。

注意事项
安装成功后，在命令行窗口里面输入 python，安装的 python 不会执行，依然会弹出 App Store 让你安装。

原因：在系统环境变量的 Path 里面，需要把当前安装的 Python 的路径填入，并且要放到 
~~~
C:\Users\MyUserName\AppData\Local\Microsoft\WindowsApps
~~~
在上述路径底下，对应的 DLL 也存在。因此，Python4Delphi 的程序能够正确加载 Python310.dll 然后正常运行。

## 几个命令
1. 在命令行下输入 python -m file 查看当前 python 安装的路径

2. 在命令行下输入 python 运行进入 python 提示符环境下，可以使用 python 的代码查看路径：
~~~
import sys
print(sys.version)
print(sys.path)
~~~

## 问题现象
错误提示：没有 PIL　这个模块。

运行 Python4Delphi 自己带来的 Demo 的 Demo29，需要 Python 安装图像处理库 PIL，在 Python3 是安装 Pillow 这个玩意。但是当 Windows APP Store 的 Python3 安装以后，因为 Delphi 的程序无法调用 App Store 安装的 Python，只能调用到自己从官网下载的安装程序安装的 Python，因此，导致在通过 Power Shell 命令行里面输入 pip install Pillow 安装成功后，运行这个 Demo 还是提示找不到 PIL 模块。

卸载 APP STORE 里面安装的 Python，手动下载安装 Python 以后，该问题解决。
