# Delphi和微软的Azure云存储

Delphi 针对云存储的支持，支持了亚马逊的 AWS 和微软的 Azure，不过经过测试，发现对 Azure 的支持是有问题的。然后俺深入研究了一下如何用 Delphi 的代码去访问 Azure 的 Blob -- Azure的云存储叫做 Blob。

**物料准备**：

Delphi 10.3 社区版。虚拟机 VittualBox 安装 WIN7。

在调试阶段，可以在自己电脑里面，或者虚拟机里面，安装微软官方的 Azure 模拟器。同时，需要安装微软官方的 Azure Resource Manager 客户端GUI工具，这样方便测试和调试。

模拟器只接受来自 127.0.0.1 的访问。如果模拟器安装在虚拟机里面，而你又要从外面去访问它，则需要做一个端口转发。我在 WIN7 虚拟机里面，采用网上查到的 WINDOWS 的 Netsh 命令为系统增加端口转发的方法，发现命令执行成功，也可以用 netsh 命令查到新增加的端口转发表，但是，转发并没有开启来。用 Netstat 查不到打开了转发端口被打开监听。因此，我自己用 Delphi 写了一个端口转发程序 -- 很简单，现成的控件。

模拟器的 Blob 工作在端口 10000 上面。访问它的地址是：127.0.0.1:10000。

因为端口转发程序是我自己写的，当然就可以把路过的数据都抓下来保存为文件。这样我可以知道访问成功的情况下，究竟是什么数据；或者访问失败的情况下，究竟是什么数据。这样可以知道我自己的程序发出的请求的具体内容。
模拟器只支持 HTTP 而不支持 HTTPS。HTTP 是明文，抓下来，正好可以学习一下访问 Azure 需要发送什么内容。

## Azure Blob 概念：
Azure Blob 是微软提供的云存储。有三种：Block Blob, Page Blob 和 Table Blob。具体区别可以看微软的官方文档的解释。

假设我要上传很多文件到这个云存储里面，采用 Block Blob。每一个文件，叫做一个 Blob。

在一个 Azure 账户底下，首先要有 Container。作为账户拥有者，你可以创建很多 Container。Container 可以看作是一个存放文件的文件夹。

在一个 Container 底下，还可以创建文件夹。但是，对于 Container 来说，文件夹和文件，不是树形的，而是平板的。

如果将一个 Container 的 public 访问权限打开，则可以通过 http://xxx.xxx.xx/yourContainer/yourFolder/ExampleFile.jpg 的方式，直接用浏览器打开看见。

通过使用 Azure 资源管理器去访问模拟器，并抓数据下来看，发现它访问模拟器的内容是 HTTP 文本。

### Azure 概念 -- 之访问者的身份验证：
Azure 对访问者的身份验证，有两种模式：

1. Shared Key 模式。其实就是采用【用户账号+用户KEY 的方式】。
2. SAS 模式。

#### Shared Key 模式：
你的账户，如果要开放给别人使用，不推荐使用这个模式。因为这个模式，你要把你的账户KEY给别人。在这个模式下，需要做：

1. 在 HTTP 的头里面，增加一些字段，描述这个请求的一些参数；
2. 按照 Azure 的规矩，构造一些请求字段，构成一个签名字符串，这个签名字符串包括了账户和账户的KEY。然后将这个签名字符串，采用 SHA2 的算法，使用账户KEY作为HASH的参数，做成一个签名HASH串。将这个串放到 HTTP 的头里面的一个名为 authorization 的字段里面。这样，在 HTTP 内容里面，就没有你的 KEY，只有使用这个 KEY 计算HASH的签名串。在 HTTP 传输过程中，这个 KEY 不会出现。
3. 上述签名字符串里面有一个比较关键的字段：时间。关于签名字符串的详细规则，后面再说。
4. 如果你的 HTTP 请求的头里面的字段和签名字符串的字段不一致（比如时间字段），则签名验证无法通过，服务器不响应这个请求。

#### SAS 模式：
你有一个账户，也就是你有这个账户的AccountName和KEY，用这个账户名和KEY在 Azure 资源管理器上创建一个对此账户的连接。在资源管理器里面，鼠标右键点击这个账户的 Blob container，选择下拉菜单里面的 Get shared access signature，创建一个 SAS key。或者别人给你一个他的账户的某个 Container 的 SAS 串。有了这个串，你才有访问这个 Container 的权限。

在 SAS 模式下，不需要去计算和创建签名字符串。直接把 SAS 串，放到请求的 URL 的后面就可以了。这里有个用浏览器访问Azure模拟器的例子 URL：

http://192.168.6.5:801/devstoreaccount1/pcplayer1?restype=blob&comp=list&maxresults=1000&delimiter=%2F&st=2019-02-01T03%3A23%3A00Z&se=2019-02-08T03%3A23%3A00Z&sp=rwdl&sv=2018-03-28&sr=c&sig=3S%2Bk6wPfndLWKVEFW1Ab1Nlrh%2BJFkYXg6SQKS7st%2Bx8%3D

通过使用浏览器访问上述 URL，然后抓取这个 HTTP 访问的内容数据，然后在 Delphi 底下，直接使用 TIdTCPClient ，采用 TCP 的方式将 HTTP 的内容部分发送给 Azure 模拟器，然后从 TCP 读取其返回值，测试成功。因此，我们可以根据这个 HTTP 访问的内容文本，通过平凑字符串，然后用 TCP 直接发送字符串的方式，完成一次对 Azure 的 HTTP 的访问。这就是官方文档所谓的 REST 访问。

### 实际操作：
对于一个真实的 Azure Blob 的操作，官方文档说它支持 HTTP 和 HTTPS。实际操作可能包括：list 操作；上传文件的操作。list 的操作是 HTTP 的 GET 命令。上传文件的操作是 HTTP 的 PUT 命令。

对于上传文件来说，构造一个 HTTP 的 PUT 命令，包括一些 HTTP 头的字段，Content-Length 字段是必须的。在 HTTP 头的最后，加多一个空行（回车符和换行符），然后紧跟着是要上传的文件的二进制数据（无需做 BASE64编码或者 MIME 编码）。在这个 HTTP 的 PUT 命令（HTTP 的第一行）的结尾部分，加上 SAS 串（PUT 部分的字符串，其实就是浏览器的 URL，只是不包含 HOST 部分）。

### 使用Delphi操作Azure
Delphi 提供了一个 TAzureBlobService 类。这个类有很多访问 Azure Blob 的方法。看起来这个类只支持 Shared key 模式，因为没有看到它有 SAS 模式的相关属性。采用这个类去访问模拟器，会报签名错误。实际上是这个类在访问模拟器的时候构造的签名字符串有问题（也许选择它的模拟器模式不会有问题，但它的模拟器模式，它固定访问127.0.0.1，而我是在虚拟机外面运行程序，通过虚拟机里面的端口转发程序去访问模拟器，这样我才可以通过用端口转发程序记录数据的功能看看它到底发送了什么数据）。总之，我使用 TAzureBlobService 去访问模拟器，没有成功过。虽然失败，但模拟器返回的 HTTP RESPONSE 很有意思，它提示了签名字符串应该是什么，如下：
~~~
<AuthenticationErrorDetail>The MAC signature found in the HTTP request 'rBeHwZfFQXNXBejxTEHoTsJRAFdGtYP8f9fQbwjvnvo=' is not the same as any computed signature. Server used following string to sign: 'GET

x-ms-date:Wed, 30 Jan 2019 15:12:26 GMT

x-ms-version:2015-02-21

/devstoreaccount1/devstoreaccount1/pcplayer1

comp:list

restype:container

timeout:30'.</AuthenticationErrorDetail>
~~~
 

它给出来的这个签名字符串，连 GET 后面跟的换行符的个数，都是正确的（GET 后面有一个换行符，然后又11个空行，也就是还有11个换行符）。直接拿这个签名字符串作为模板，构造自己的签名字符串，唯一需要的就是替换掉里面的时间值，也就是 x-ms-date 字段的值。这个值可以由 TAzureBlobService 里面的计算时间值的代码直接复制过来使用，代码如下：
~~~
function TForm1.GetXMsDate: string;
 
begin
 
  //function TAzureService.XMsDate: string;
 
  Result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"',
 
  TTimeZone.Local.ToUniversalTime(Now),
 
  TFormatSettings.Create('en-US'));
 
end;
~~~

签名字符串准备好以后，采用以下函数对其计算 HASH：
~~~
function TForm1.SignString(const Signkey: TBytes;
 
const StringToSign: string): string;
 
var
 
  B: TBytes;
 
begin
 
  //为字符串签名 TCloudSHA256Authentication.SignString
 
  //这里的 SignKey 就是那个 AccountKey
 
  //这里需要拿来签名的字符串是 StringToSign，其内容是 HTTP 里面的头。包括时间戳。
 
 
 
  B := THashSHA2.GetHMACAsBytes(StringToSign, Signkey, THashSHA2.TSHA2Version.SHA256);
 
  Result := TNetEncoding.Base64.EncodeBytesToString(B);
 
end;
~~~
上述计算方法，也来自 TAzureBlobService，测试有效。

#### 使用 IdTCPClient 控件
将 TAzureBlobService 访问模拟器的数据抓下来，里面就是 HTTP 的内容，直接拿这个内容作为模板，替换掉里面的时间字段的值，以及签名串，然后用 TIdTCPClient 直接发给模拟器，模拟器返回成功！
上述方法是 **List** 一个 BLob。上传一个文件，也是类似操作。这里的 List 是 Azure 命令。

因为 DELPHI TAzureBlobService 不支持 SAS 模式，因此 SAS 模式需要自己做实验。

#### 使用TIdTCPClient访问Azure的ASA模式
SAS 模式访问模拟器：

首先，通过组合字符串的方式将 SAS 串加到 URL 后面，然后在浏览器地址栏输入这个 URL 直接访问模拟器，成功。

https://docs.azure.cn/zh-cn/storage/common/storage-dotnet-shared-access-signature-part-1?toc=%2fstorage%2fblobs%2ftoc.json 这篇文章讲了 SAS 模式。

先测试 SAS 方式做 BlobList，构造一个 URL：

http://192.168.6.5:801/devstoreaccount1/pcplayer1?restype=container&comp=list&maxresults=1000&delimiter=%2F&st=2019-02-01T03%3A23%3A00Z&se=2019-02-08T03%3A23%3A00Z&sp=rwdl&sv=2018-03-28&sr=c&sig=3S%2Bk6wPfndLWKVEFW1Ab1Nlrh%2BJFkYXg6SQKS7st%2Bx8%3D

这个 URL，使用了我昨天构造的 SAS KEY，也就是上述 URL 的 【st=2019-02-01T03%3A23%3A00Z&se=2019-02-08T03%3A23%3A00Z&sp=rwdl&sv=2018-03-28&sr=c&sig=3S%2Bk6wPfndLWKVEFW1Ab1Nlrh%2BJFkYXg6SQKS7st%2Bx8%3D】这部分。

上述 URL，抓包看，HTTP 内容部分，有 HTPP 的头，以下是抓包到的头：
~~~
GET /devstoreaccount1/pcplayer1?restype=container&comp=list&maxresults=1000&delimiter=%2F&st=2019-02-01T03%3A23%3A00Z&se=2019-02-08T03%3A23%3A00Z&sp=rwdl&sv=2018-03-28&sr=c&sig=3S%2Bk6wPfndLWKVEFW1Ab1Nlrh%2BJFkYXg6SQKS7st%2Bx8%3D HTTP/1.1

Host: 192.168.6.5:801

Connection: keep-alive

Upgrade-Insecure-Requests: 1

User-Agent: Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36

Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8

Accept-Encoding: gzip, deflate

Accept-Language: zh-CN,zh;q=0.9
~~~

采用 TIdTCPClient 直接用 TCP 方式将上述 HTTP 内容发送给模拟器，返回成功！
上传文件的，其 HTTP 是 PUT，格式请参考以下代码：
~~~
procedure TForm1.PutBlobSAS_TCP;
 
var
 
  SL: TStringList;
 
  S: string;
 
  B: TBytes;
 
  FileStream: TFileStream;
 
begin
  //采用 SAS 模式，构造一个 PUT BLOB
  //本代码测试成功。成功上传了一个图片文件到 Azure 模拟器里面
  SL := TStringList.Create;
  try
    SL.Add('PUT /devstoreaccount1/pcplayer1/picture/cpu.jpg?timeout=30&st=2019-02- 
     01T03%3A23%3A00Z&se=2019-02-08T03%3A23%3A00Z&sp=rwdl&sv=2018-03- 
     28&sr=c&sig=3S%2Bk6wPfndLWKVEFW1Ab1Nlrh%2BJFkYXg6SQKS7st%2Bx8%3D HTTP/1.1');
 
    SL.Add('Connection: Keep-Alive');
 
    SL.Add('Content-Type: application/x-www-form-urlencoded');
 
    //SL.Add('Authorization: SharedKey devstoreaccount1:##SHA'); //采用了 SAS，这一行不能要。否则服务器仍然按照这一行去验证 KEY。
 
    SL.Add('User-Agent: Embarcadero URI Client/1.0');
 
    SL.Add('x-ms-blob-type: BlockBlob');
 
    //SL.Add('x-ms-date: ' + Self.GetXMsDate); //SAS 模式下，这个时间戳看起来不需要。
 
    SL.Add('x-ms-meta-MyMetaData: pcplayer');
 
    SL.Add('x-ms-version: 2015-02-21');
 
    SL.Add('Content-Length: 40490');
 
    SL.Add('Host: 192.168.6.5:801');
 
    SL.Add(''); //加多一个空行，表示 HTTP 头结束。
    B := BytesOf(SL.Text); //注意这里是否会丢掉最后一个回车换行符？
    Self.IdTcpDoHttp(B, False); //发送页面头部分
  finally
  SL.Free;
 end;
end;
 
//接着发送文件内容二进制数据部分
 
  FileStream := TFileStream.Create('D:\tt\cpu.jpg', fmOpenRead);
 
  try
    SetLength(B, FileStream.Size);
    FileStream.Position := 0;
    FileStream.Read(B[0], FileStream.Size);
    S := Self.IdTcpDoHttp(B, True);
    Memo1.Lines.Add(S);
  finally
    FileStream.Free;
  end;
end;
~~~

其中，http 头部分，直接参考了采用 Shared key 模式的上传页面的 HTTP 头。只是一定要去掉 'Authorization: SharedKey devstoreaccount1:##SHA‘ 这一行。否则 Azure 仍然按照这个 Shared key 方式去验证签名。

另外，之前 Shared key 模式下的 http 头里面必须要有的时间戳字段 'x-ms-date: ' + Self.GetXMsDate’ 也不是必须的。有没关系，没有也不影响上传。

这里和 Shared key 模式不同的地方是，对于身份验证，不需要去构造一个签名字符串然后计算其 HASH 然后放到 HTTP 头里面。只需要在 HTTP GET 的参数的结尾部分放入 SAS 串就可以了。

注意，上述 HTTP 头里面有一个 'x-ms-meta-MyMetaData: pcplayer' 这种客制化的 MetaData，对这个文件的描述。上传成功后，采用 Azure 资源管理器可以看到这个文件，并且它的 MetaData 确实有这个客制化的数据。因此我们可以通过这个方式为这个文件加上很多说明信息。

对于真实的 Azure 的上传文件的，最好用 HTTPS。这里我就直接使用 Delphi 自带的 TNetHTTPClient 这个专门用于 HTTP 的控件，将它的 SecretProtocols 属性都选中，这样它就自动支持 HTTPS 了。采用这个控件，让它发送上面那个 URL，当然 HOST 部分要改为真实环境的值。以下是测试成功的代码：
~~~
procedure TForm1.Button7Click(Sender: TObject);
var
  AHeaders: TNetHeaders;
  URL: string;
  AResponseContent: TStringStream;
begin
  //采用 NetHttpClient 去访问真实的 Azure;
  AResponseContent := TStringStream.Create;
 
  URL := 'https://Mystore.blob.core.windows.net/study-test/cpu.jpg?timeout=30&sv=2018-03-28&ss=b&srt=sco&sp=rwdlac&se=2019-03-02T04:45:10Z&st=2019-01-01T20:45:10Z&spr=https&sig=cfYUJSBoi2%2BOZsJLRslSBDvwAafdVYcxrngKXPgaGsI0%3D';
  NetHTTPClient1.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded';
  NetHTTPClient1.CustomHeaders['x-ms-blob-type'] := 'BlockBlob';
  NetHTTPClient1.CustomHeaders['x-ms-meta-MyMetaData'] := 'My_test_Data';
  NetHTTPClient1.CustomHeaders['x-ms-version'] := '2015-02-21';
  NetHTTPClient1.CustomHeaders['Content-Length'] := '40490';
  //NetHTTPClient1.CustomHeaders('')
 
  NetHTTPClient1.Put(URL, 'D:\tt\cpu.jpg', AResponseContent, nil);
 
  Memo1.Lines.Add(AResponseContent.DataString);
  ShowMessage('PUT 完成！');
end;
~~~

注意 HTTP 头里面的 Content-Length 的值是 40490，这个是我上传的文件 cpu.jpg 的实际大小。

上述代码，上传一个文件到真实的 Azure 账户里面，而不是模拟器，成功！

## Azure 总结：

1. Azure 有两种权限验证模式。  
    1.1. Shared key 模式。使用这个模式，需要自己组装一个签名字符串，然后采用 SHA 对这个签名字符串做 HASH，将这个 HAS 字符串放到 HTTP 头里面。  
    1.2. SAS 模式。直接将账户拥有者为一个 Container 创建的 SAS 串，放到 HTTP 请求的 URL 的后面就可以了。无需再做其它事情。
2. 对 Azure Blob 云存储的访问，是 HTTP 或者 HTTPS。对于 HTTP 或者 HTTPS，完全可以自己构建其内容，采用 TCP 发送给 Azure 然后读回其 HTTP RESPONSE。  
    2.1. 请求一个 Blob 的 List 等操作，是 HTTP 的 GET 操作；  
    2.2. 上传一个文件，是 HTTP 的 PUT 操作。上传的文件的二进制数据直接跟在 HTTP 头后面。HTTP 头的后面要有一个空行，隔开上传文件的二进制数据。
3. 上传一个 Block Blob 就是上传一个文件。在 HTTP 头里面，还可以增加一些自己定义的头字段，用于描述这个文件，Azure 会记录下这些信息。这个是 Azure 对一个 Blob 的 MetaData 描述信息。  
    3.1. HTTP 头就是 NAME=VALUE格式的字符串。只不过中间不是【=】等号，而是【：】冒号。

结束。