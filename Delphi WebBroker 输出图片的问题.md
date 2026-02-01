# Delphi WebBroker 输出图片的问题

Delphi 的 WebBroker 是一个 http server，可以直接在里面写代码响应来自浏览器的请求，输出网页给浏览器。

在 WebBroker 里面，如果要输出图片，可以将图片内容加载到一个 TStream 里面，然后：
  ~~~
  Response.ContentType := 'image/jpeg';
  
  Response.SendStream(AStream);
  ~~~
这样输出。如果用 Windows 的 IE 浏览器来访问，没有问题。用安卓手机的浏览器来访问，也没问题。

但是，iOS 手机的浏览器有问题，无法看到图片。Windows 下的 Chrome 浏览器也有问题。Chrome 给出的错误提示：**ERR_INVALID_HTTP_RESPONSE**。

朋友建议抓包看。结果一抓包，发现 Server 给出的响应，居然没有 http 头，而是直接给出了图片的二进制数据。

试着改一下代码：
  ~~~
    Response.ContentStream := AStream;
    
    Response.SendResponse;
  ~~~
改完代码再次测试，Chrome 浏览器没问题了。iOS 底下也没问题了。

查 delphi 关于 SendStream 的帮助，里面确实说到，调用 SendStream 之前，要先调用 SendResponse； SendResponse 会先发送 http 响应信息。