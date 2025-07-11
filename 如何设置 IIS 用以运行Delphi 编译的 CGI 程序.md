# 如何设置 IIS 用以运行Delphi 编译的 CGI 程序
# 使用 Delphi 的 WebBroker 架构，可以非常方便地开发 Web 服务器程序。
结合一些好的前端库，可以很简单地作出非常漂亮功能强大的基于 WEB 页面的程序。

具体做法这里就不细说了。

在 Delphi 里面新建一个 Web Server 的工程，选择 IIS CGI 以后，这个工程编译出来的就是一个 Windows 的 Exe 可执行文件，它就是可以在 IIS 底下被 IIS 调用的 CGI 程序。

额外说一句：Delphi 还可以编译出基于 Apache 的跑在 Linux 服务器上的 Web 服务器程序。

# 程序有了，如何让 IIS 能够调用它？
假设我的 CGI 程序的名字叫做：testCGI.exe；你在 Windows 里面如果直接双击它，它就会运行，只不过弹个黑色窗口出来，一瞬间，然后就运行完成，黑色窗口消失了。

## 首先，电脑上得安装 IIS
## 其次，在安装 IIS 的时候，要勾选上 CGI
上面两步，都是 Windows 的控制面板里面，【程序和功能】-- 【启用或关闭 Windows 功能】，从这里去找到 IIS 相关的东西，勾选上，就可以安装了。

IIS 有了，在 Windows 控制面板里面选择【系统】，进去后选择【Windows 工具】，路径：控制面板\系统和安全\Windows 工具

在这里可以看到 【Internet imformation service(IIS) 管理者】，双击它打开，就是 IIS 的管理面板。

先把我的 CGI 程序放到硬盘某个目录下，我这里是：F:\CGI\；我的程序名：testCGI.exe

## 创建虚拟目录
IIS 安装完成后，默认的 WEB 根目录在：C:\inetpub\wwwroot

假设你在这个目录底下放一个 abc.html，然后浏览器访问：http://localhost/abc.html，就能看到对应的网页。

但是，我不想让这个目录页执行CGI的权限。因此我的程序放在了上面说的 F 盘。

在 IIS 管理面板里面，选择左侧【网站】底下的 【Default Web Site】，右键点击，下拉菜单，选择【添加虚拟目录】。在弹出来的窗口里，我填写虚拟目录的名字：【test】，然后设置这个虚拟目录指向：F:\CGI；

设置完成后，如果在 F:\CGI 里面放一个 abc.html，然后浏览器访问 http://localhost/test/abc.html 能够正确打开网页，说明设置成功。

但是，这时候用浏览器访问 http://localhost/test/testCGI.exe 多半是不会成功的。因为还没有 CGI 的执行权限。

## 开启 CGI 运行权限
右侧面板选择 test 这个虚拟目录，面板中间一堆图标里面，选择【处理程序映射】，双击，打开了处理程序映射到界面，就是一个表格。表格里面有一些项目，可能也有一个项目名称叫【CGI-EXE】。没有也没关系，鼠标右键菜单，手动添加一个。

关键是，这个 CGI 的项目，状态值是【禁用】。双击它，弹出来的面板里面，没有【启用】的选项或者勾选框。

这里，是在表格里面选择它，然后在面板的右侧菜单里面，去找【编辑功能权限】，点这个菜单，弹出来的框里面，有三项：读，写，执行；其中执行没有打勾。勾选上执行，搞定。

到这里，在浏览器里面输入：http://localhost/test/testCGI.exe 能够看到我自己写的测试程序输出的页面内容；

再来：

http://localhost/test/testCGI.exe/hello?id=1234

也能看到我的程序的正确的执行结果，也就是执行到程序内部的 /hello 的结果。一切都那么美好。

## 但是，我不想让人看到是在执行 testCGI.exe 这个程序。怎么办？
目标：

我想让浏览器地址栏输入：http://localhost/test/hello?id=1234 能看到和上面写了 testCGI.exe 一样的效果。

同样，访问到我的程序的根，也就是：

http://localhost/test/testCGI.exe

我想要这样的 URL：

http://localhost/test/

**有什么办法？**

## 使用 URL 重写
IIS 默认没有 URL 重写。需要单独下载一个 URL 重写模块，运行安装程序。微软官方下载地址，网上随便就能搜到。

安装完成后，在 IIS 管理面板的中间一大堆图标里面，就能看到一个图标：【URL重写】

好了，开始设置。这个设置我摸索了几个小时，网上查了一堆资料，都没看明白。但真搞明白了，其实很简单，注意点就一条。

URL 重写设置具体操作

1. 首先，IIS 管理面板右边，选择 test 这个虚拟目录上一层的【default web site】。这里千万注意，一开始我选择的是 test 本身，结果设置完成后，什么效果都木有 -- 就这一个注意点。

2. 这时候面板中间有个【URL 重写】的图标，双击它，IIS 管理面板进入 URL 重写的界面。就是一个表格。这个表格上空的，什么项目都没有。

3. 鼠标在表格里面右键，弹出菜单里面现在【添加规则】；出来一个框，选择【入站规则，空白规则】，其实不用选择，默认的，直接点按钮，就弹出来规则填写框。在里面填写规则。

4. 重点来了，这里总共有 4 条要填写：

4.1. 名称，这个随便填写。我怕中文不支持，就填了个 test；

4.2. 底下有一栏，标题是模式，是一个输入框。对于我的 test 虚拟目录来说，填写："^test/(.*)"，这里，各位填写的时候，把 test 改成自己的虚拟目录名称，不要把两边的双引号给填进去了。

4.3. 再下面，有个标题：【重写URL】的输入框。这里填入：test/testCGI.exe/{R:1}

4.4. 再下面，有一个【停止处理后继规则】，勾选上。

# Done.