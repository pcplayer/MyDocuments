# 显示 Markdown 的文件

使用 Markdown 格式来写文本很流行。这里探讨的是如何在我们自己用 Delphi 代码写的程序里面，去显示 Markdown 格式的文本文件。
**主要目标**：
1. 文件里面包含图片。Markdown 格式的图片是写的图片文件名的引用。本程序默认图片和文本文件在同一个目录下；
2. 文件包含代码。显示的时候需要代码高亮。

## 如何将 Markdown 的文本内容解析为 HTML
这里使用开源的 https://github.com/EtheaDev/MarkdownProcessor

## 如何显示图片
Markdown 文本里面的图片，被转换为 HTML 格式的文本后，图片描述变成了 <img src="MyPic.jpg" /> 这样的标记。如果我把转换后的 HTML 保存为文件，并且这个 HTML 文件和原来的 Markdown 文件在同一个文件夹底下，也就是和图片文件在同一个文件夹底下，则浏览器加载这个 HTML 文件后，应该能够加载并显示图片。

但是，我不想在文件夹里面产生额外的文件。我想把转换后的 HTML 内容作为字符串直接让浏览器显示。因此，就需要把 <img src="MyPic.jpg" /> 这样的标记，替换为 <img src="data:image/jpeg;base64,xxxxx"> 这样的内容。

为此，这里专门写了一段代码用于对图片进行处理。代码如下：
~~~
unit UImageHandle;
{-----------------------------------------------------------------------
  这个单元的功能：
  一个 HTML 的文本，里面有多个 <img src="mypic.jpg" /> 这样的图片，
  全部找出来，把 "MyPic.jpg" 替换为图片对应的 Base64 的数据，变成：
  <img src=<img src="data:image/jpeg;base64,' + Self.FMyPicStr + '"> 其中 FMyPicstr 是图片的 Base64 数据。

  这样就直接把图片嵌入到 HTML 文本里面了。
  用途：Markdown 文本里面的图片描述，转化为 HTML 格式后，直接插入图片数据，给浏览器显示。

  技术：这里使用 Delphi 的正则表达式 System.RegularExpressions 来进行字符串的搜索和替换。

  用法：直接调用 function ReplaceImgSrcWithBase64(const HtmlText, BasePath: string): string; 函数完成替换功能。
  其中，BasePath 是图片文件的目录。程序在这个目录下搜索 MyPic.jpg 文件，加载文件，编码为 Base64；

  pcplayer 2025-11-13
---------------------------------------------------------------------------}
interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  System.NetEncoding,
  System.IOUtils,
  System.StrUtils,
  System.Math;

type
  TImgReplacer = class
  private
    FBasePath: string;
    function ImageFileToBase64(const FileName: string): string;
    function GetMimeTypeByExt(const Ext: string): string;
    function StripQueryAndFragment(const UrlOrPath: string): string;
  public
    constructor Create(const ABasePath: string);
    // 注意：这是一个实例方法，匹配 TMatchEvaluator 的 "of object" 签名
    function EvalMatch(const Match: TMatch): string;
  end;

function ReplaceImgSrcWithBase64(const HtmlText, BasePath: string): string;

implementation

{ TImgReplacer }

constructor TImgReplacer.Create(const ABasePath: string);
begin
  inherited Create;
  FBasePath := ABasePath;
end;

function TImgReplacer.EvalMatch(const Match: TMatch): string;
var
  Prefix, ImgPath, Suffix, CleanPath, FullPath, Base64Str, MimeType, Ext: string;
begin
  // Pattern 保证有三个捕获组
  Prefix := Match.Groups[1].Value;
  ImgPath := Trim(Match.Groups[2].Value);
  Suffix := Match.Groups[3].Value;

  // 跳过 data: 与绝对 HTTP(S) URL
  if StartsText('data:', ImgPath) or StartsText('http://', LowerCase(ImgPath)) or
     StartsText('https://', LowerCase(ImgPath)) then
  begin
    Result := Match.Value;
    Exit;
  end;

  // 去掉查询与 fragment，再取扩展名
  CleanPath := StripQueryAndFragment(ImgPath);
  Ext := LowerCase(ExtractFileExt(CleanPath));

  // 如果 CleanPath 是绝对文件路径，直接用；否则按 BasePath 组合
  if TPath.IsPathRooted(CleanPath) then
    FullPath := CleanPath
  else
    FullPath := TPath.Combine(FBasePath, CleanPath);

  Base64Str := ImageFileToBase64(FullPath);

  if Base64Str <> '' then
  begin
    MimeType := GetMimeTypeByExt(Ext);
    // 保留原始的其它属性，替换 src 值
    Result := Prefix + Format('src="data:%s;base64,%s"', [MimeType, Base64Str]) + Suffix;
  end
  else
    // 读取失败或文件不存在，保留原始标签
    Result := Match.Value;
end;

function TImgReplacer.GetMimeTypeByExt(const Ext: string): string;
var
  e: string;
begin
  e := LowerCase(Ext);
  if (e = '.jpg') or (e = '.jpeg') then Exit('image/jpeg');
  if e = '.png' then Exit('image/png');
  if e = '.gif' then Exit('image/gif');
  if e = '.bmp' then Exit('image/bmp');
  if e = '.svg' then Exit('image/svg+xml');
  Result := 'application/octet-stream';
end;

function TImgReplacer.ImageFileToBase64(const FileName: string): string;
var
  Bytes: TBytes;
begin
  Result := '';
  if (FileName = '') then Exit;
  if not TFile.Exists(FileName) then Exit;
  Bytes := TFile.ReadAllBytes(FileName);
  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
end;

function TImgReplacer.StripQueryAndFragment(const UrlOrPath: string): string;
var
  pQuestion, pHash: Integer;
begin
  if UrlOrPath = '' then Exit('');
  pQuestion := Pos('?', UrlOrPath);
  pHash := Pos('#', UrlOrPath);
  if (pQuestion = 0) and (pHash = 0) then
    Result := UrlOrPath
  else
  begin
    if (pQuestion = 0) then pQuestion := MaxInt;
    if (pHash = 0) then pHash := MaxInt;
    Result := Copy(UrlOrPath, 1, Min(pQuestion, pHash) - 1);
  end;

end;

{ 辅助函数：调用 TRegEx.Replace 使用实例方法作为 Evaluator }
function ReplaceImgSrcWithBase64(const HtmlText, BasePath: string): string;
var
  Pattern: string;
  Replacer: TImgReplacer;
  Evaluator: TMatchEvaluator;
begin
  // 捕获三部分：前缀、src 值、后缀（保留其它属性）
  Pattern := '(<img\s+[^>]*?)\bsrc="([^"]+)"([^>]*>)';
  Replacer := TImgReplacer.Create(BasePath);
  try
    // 直接把实例方法赋给 Evaluator（符合 "of object"）
    Evaluator := Replacer.EvalMatch;
    // 使用你要求的静态重载（Evaluator 为 method pointer）
    Result := TRegEx.Replace(HtmlText, Pattern, Evaluator);
  finally
    Replacer.Free;
  end;
end;

end.
~~~

## 代码高亮
代码高亮需要一些外部的 CSS 框架和 JavaScript 库。为了不在程序里写死，单独写了一个 html 模板文件。程序加载这个模板文件，再把从 Markdown 文件转换来的 HTML 内容插入到页面模板中。
模板页面的代码如下：
~~~
<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <title>GitHub Style with Highlight.js</title>

  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown.min.css">

  <!-- 可选：GitHub 风格语法高亮 -->
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/highlight.js@11.9.0/styles/github.min.css">
  <script src="https://cdn.jsdelivr.net/npm/highlight.js@11.9.0/lib/highlight.min.js"></script>
  <script defer>
  document.addEventListener("DOMContentLoaded", function() {
    hljs.highlightAll();
  });
</script>
  

  <style>
    body {
      display: flex;
      justify-content: center;
      background-color: #f6f8fa;
      padding: 40px;
    }
    .markdown-body {
      box-sizing: border-box;
      max-width: 800px;
      background: white;
      padding: 30px;
      border-radius: 8px;
    }
  </style>
  
  <script language="JavaScript">
	function WriteContent(htmlstr) {
	
	  var mycontent = document.getElementById("MyContent");
	  mycontent.innerHTML =htmlstr;
	}
 </script>
</head>
<body>
  <article class="markdown-body">	
	<div id="MyContent">#MyContent</div>  
  </article>
  

</body>
</html>
~~~

上述页面模板文件，保存到程序目录下，文件名：Template.html；

## TEdgeBrowser 的用法
HTML 内容的显示，使用 TEdgeBrowser。
1. 需要把 WebView2Loader.dll 这个文件，放到程序运行的目录下；
2. 需要初始化它，代码如下：
~~~
procedure TFmEdgeMarkdown.FormCreate(Sender: TObject);
begin
  EdgeBrowser1.CreateWebView; //需要在这里初始化 EdgeBrowser 否则 EdgeBrowser1.NavigateToString(HtmStr); 不会显示内容。
end;
~~~

如果没做初始化，单纯地写入 HTML 字符串，它不会有异常错误提示，也不显示任何内容。

## 最后的代码
~~~
procedure TFmEdgeMarkdown.LoadMarkDownFile(const Fn: string);
var
  SL: TStringList;
  S, TempStr, APath, TempFn: string;
  Processor: TMarkdownProcessor;
begin
  APath := ExtractFilePath(Fn);

  SL := TStringList.Create;
  try
    SL.LoadFromFile(Fn, TEncoding.UTF8);  // UTF8 内容的文本文件，加载后使用 UTF8Decode(SL.Text) 获得的字符串会有乱码；使用 TEncoding.UTF8 参数则不会。
    S := SL.Text;
  finally
    SL.Free;
  end;

  Processor := TMarkdownProcessor.CreateDialect(TMarkdownProcessorDialect.mdCommonMark);
  try
    S := Processor.Process(S);
    S := ReplaceImgSrcWithBase64(S, APath); //UImageHandle.pas 单元的函数
  finally
    Processor.Free;
  end;

  TempFn := TPath.Combine(ExtractFilePath(Application.ExeName), 'Template.html');

  SL := TStringList.Create;
  try
    SL.LoadFromFile(TempFn);
    TempStr := SL.Text;
  finally
    SL.Free;
  end;

  //复杂内容使用 JavaScript 写入页面会出问题。干脆直接用 Delphi 代码组装页面。
  S := TempStr.Replace('#MyContent', S);
  Self.ShowHTML(S);
end;

procedure TFmEdgeMarkdown.ShowHTML(const HtmStr: string);
begin
  EdgeBrowser1.NavigateToString(HtmStr);
end;
~~~

## 最后注意
因为众所周知的原因，模板页面里面关于代码高亮引用的 CDN 上面的 CSS 和 JS 库，可能加载不了，导致的现象就是页面一片空白，没有显示内容。
此时，科学上网，就能看到正确内容。

当然，如果仅仅是显示来自 Markdown 文件的内容，不需要代码高亮，则不需要加载页面模板，直接把转换自 Markdown 文件的 HTML 内容发送给 EdgeBrowser1 就能看到正确的内容。
