# 基于内置 WebServer 的 Markdonw 预览

1. 把 Markdown 文本的 Markdown 标记，转换为 HTML 标记；
2. 使用浏览器来显示 HTML；
3. 浏览器是通过访问本程序内置的 Web Server 获得要显示的文件内容。

## 需求一：代码高亮
需要使用 CSS 和 JS；因此，需要一个 HTML 页面模块框架。最终输出的页面，是把 Markdown 文本转换后的 HTML 内容，嵌入这个页面模板，发送给客户端；

## 需求二：图片
Markdown 文本里面可能有插入的图片。转换为 html 以后就是一个类似：
~~~
  <img src="abc.jpg" /> 
~~~
的图片引用。浏览器会根据引用的地址去加载图片。

## 内置的 Web Server
### 创建一个内置 Web Server 的工程
1. Delphi - New - Other - Web - Web Server Application；
2. 选择 Stand Alone 模式，VCL，Windows；
然后 Delphi IDE 会创建一个内置 Web Server 的 VCL 程序框架。
### 内置 Web Server 的简单说明
1. 内置的 Web Server 是基于 TIdHTTPServer 的。它在本地电脑上打开一个端口（比如：8080），等待来自浏览器的访问；
2. 内置的 WEB 代码框架是 Delphi 的 Web Broker 框架。响应浏览器的请求，只需要在 Web Broker 的 TWebModule1 里面写代码。

**代码图片**：

![WebBroker代码](WebBroker1.jpg)

### 代码需要注意的地方
1. 对于来自浏览器对 CSS/JS 文件，以及图片文件的请求，这里使用了 TWebFileDispatcher；也就是拖一个 TWebFileDispatcher 控件到 TWebModule1 里面。
2. 本软件默认 Markdown 文件里面引用的图片和该文件在同一个文件夹下。
3. 本软件默认 css / js 文件和本软件在同一个文件夹下。

因此，要本程序要根据浏览器请求的文件是什么文件，给 TWebFileDispatcher 设置目录，让它能够找到文件。代码如下：
~~~
procedure TWebModule1.WebModuleBeforeDispatch(Sender: TObject; Request:
    TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Fext: string;
begin
  Fext := Lowercase(ExtractFileExt(Request.PathInfo));

  if (Fext = '.jpg') or (Fext = '.png') or (Fext = '.bmp')  then
  begin
    WebFileDispatcher1.RootDirectory := FmMain.MarkdownFilePath; //图片路径
  end
  else
  if (Fext = '.css') or (Fext = '.js') then
  begin
    WebFileDispatcher1.RootDirectory := Self.GetTemplatePath;
  end;
end;
~~~

## 关于 Markdown 的处理
这里使用了开源的 Markdown 处理器：https://github.com/EtheaDev/MarkdownProcessor
程序中直接引用它的源代码，创建处理器对象，对 Markdown 文本进行处理，代码如下：
~~~
function TWebModule1.LoadMarkdownFileToHTML(const Fn: string): string;
var
  SL: TStringList;
  Processor: TMarkdownProcessor;
  S: string;
  TempFn, TempStr: string;
begin
  if not FileExists(Fn) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(Fn, TEncoding.UTF8);
    S := SL.Text;
  finally
    SL.Free;
  end;

  Processor := TMarkdownProcessor.CreateDialect(TMarkdownProcessorDialect.mdCommonMark);
  try
    S := Processor.Process(S);
  finally
    Processor.Free;
  end;

  //加载模板
  TempFn := Self.GetTemplatePath; // ExtractFilePath(ParamStr(0));
  TempFn := TPath.Combine(TempFn, 'Template.html');
  SL := TStringList.Create;
  try
    SL.LoadFromFile(TempFn, TEncoding.UTF8);
    TempStr := SL.Text;
  finally
    SL.Free;
  end;

  Result := TempStr.Replace('#MyContent', S);
end;
~~~

到此，本程序基本功能完成。