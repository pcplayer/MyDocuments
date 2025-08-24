# 使用 OpenSSL 为自己做证书实现自己的 HTTPS 服务器

一个面向公众的网站，如果想要对用户访问网站过程中的通讯进行加密，让户使用 HTTPS 的方式来访问，网站自己需要有自己的私钥，以及由可信任机构签发的证书。这是采用 HTTPS 访问的技术架构。

由可信任机构签发的证书，要钱。某些情况下，我们可以自己为自己签发证书。比如，开发者自己开发测试；比如，公司内部使用而不是面向公众的网络访问。

我在 Delphi 开发的基于 Indy 的 WebBroker 或者 WebService 服务器支持 https 这篇博客文章里面，讲到用自己制作的私钥和证书用于 Delphi Indy 的 Web Server 程序和 WebServices 程序。

下面简单讲一下使用 OpenSSL 制作私钥和证书的具体方法。

## 概念：
1. 用浏览器使用 https://website.com/ 的方式访问一个网站，和单纯的 http://website.com/ 的方式比较，http 的通讯是没有加密的，用 Sniffer 抓包工具或其它方式拦截到通讯内容，是可以直接阅读的。而 https 在通讯层，是基于 SSL 的加密通讯，拦截到的通讯内容，拦截者无法解密阅读。

2. SSL 加密通讯基于非对称加密技术。具体到一个支持 HTTPS 访问的网站，其网站内部需要有自己的非对称加密的私钥和包含非对称加密公钥的证书。

2.1. 证书是由可信任机构（比如 google 或者 Microsoft 或者专门做证书签发的机构）签发；因此，客户端可以信任该证书里面的公钥的确是该网站的，而不是被攻击者伪造的。

2.2. 客户端如何判断证书是可信任的？因为从证书里面包含的信息，可以追溯到签发这个证书的机构的根证书。而该根证书是作为被信任的证书添加到 Windows 操作系统里面的。当然用户也可以自己手动将这个根证书的信任去除掉。

3. 假设一个网站需要访问的人必须要提供签名证书来证明自己的身份（这种通常不是面向普通公众的网站），那么，一个普通用户，也是可以有自己的私钥和证书的。

5. 证书信任链：作为专门签发证书的可信任机构，它自己的根证书及私钥，通常是不会用来为用户签发证书的。这个根私钥的安全性非常重要，通常不拿出来使用。因此，根私钥和根证书，用于制作签发一些中间的证书。再由这个中间的证书和私钥，去签发用户需要的证书。这个类似【中间商】的角色，可以有多个层次。由此构成一个信任链条。也就是一个最终的证书，是可以追溯到根证书的。因此，验证一个证书是否合法，并不需要上网连线才能验证。只要用户的系统里面有可信任的根证书就可以验证该证书是否合法。

5.1. 但是，如果一个证书被吊销（比如它的私钥被盗），需要在线查询。证书签发机构，有一个服务器专门提供这样的查询。当然这是一个标准的协议，电脑系统会自动完成这个过程。

6. 用户可以自己制作自己的私钥以及一个用于证书签发的 CSR（certificate signing request，证书签发请求）。这样用户保留自己的私钥，仅仅需要把这个 CSR 发送给证书签发机构。证书签发机构将这个 CSR 签发成真正的证书。这样用户的私钥才不会被任何的第三方看到。

名词：
1. OpenSSL：OpenSSL 是一个免费且开源的加密库，它提供了多个用于处理数字证书的命令行工具。其中一些工具可用于充当证书颁发机构。

2. CA：certificate authority，证书颁发机构，是一个对数字证书进行签名的实体。

3. 私钥叫做Key，根的私钥（root key），其对应的文件名：ca.key.pem；公钥就是证书，其对应的文件名（根证书 root certificate）ca.cert.pem; 这里的 cert 就是 certificate 的意思。

4. CSR：certificate signing request。第三方自己创建了私钥，然后发给CA的证书签名请求。

## 实操：
### 环境搭建：
我是在 Windows 10 的 Linux 子系统里面使用 OpenSSL 的。我的 Windows 10 的 Linux 子系统是安装的 Ubuntu，安装完成后，里面就已经有了 OpenSSL。如果没有，Linux 底下如何安装 OpenSSL 网上很多文章。或者直接在 Windows 里面安装 Win 版的 OpenSSL，我估计命令行操作应该是一样的。

总之，启动 Windows PowerShell 在里面输入 bash 命令，进入 Linux。这样可以直接在里面运行 OpenSSL 了。

### 操作：
#### 一. 创建根的私钥和证书对
1. 准备工作目录。我是在当前用户下创建了一个叫 ca 的目录。在 Windows 里面，实际上是在当前 Windows 用户的目录下。

2. 在 ca 目录下创建3个文件夹：

mkdir certs crl newcerts private

 

3. 在 ca 目录下创建两个文件，输入命令：（**备注：以下的所有命令的当前路径都是这个 ca 目录**）

3.1. touch index.txt

3.2. echo 1000 > serial

4. 更改 private 目录的权限：chmod 700 private

5. 创建 root key：输入命令：openssl genrsa -aes256 -out private/ca.key.pem 4096 创建 root key。复制上述命令后，用鼠标在powershell 窗口里面点右键就粘贴进去了。回车后会提示输入密码。输入一个自己编制的密码就创建好 root key。

5.1. root key 也就是根的私钥，其文件是：ca.key.pem，在 private 目录下；

5.2. chmod 400 private/ca.key.pem

5.3. 这个 ca.key.pem 文件是加密的，以后每次使用它的时候需要输入密码。这个密码就是创建的时候系统提示你输入的密码。

6. 创建根证书：创建根证书需要 openssl.cnf 这是一个文本配置文件。

6.1. 本文最顶上那个参考链接里面，有这个 openssl.cnf 的模板，下载下来，稍做修改，或者不改，都能用。

6.2. openssl.cnf 放到 ca 目录下。输入以下命令：
~~~
openssl req -config openssl.cnf \
 
-key private/ca.key.pem \
 
-new -x509 -days 7300 -sha256 -extensions v3_ca \
 
-out certs/ca.cert.pem
~~~
 6.3. 在创建证书的过程中，我碰到错误提示：unable to load Private Key，其原因就是命令执行过程中系统提示我输入 ca.key.pem 的密码，我输入错误导致。

6.4. 校验刚才创建的证书：openssl x509 -noout -text -in certs/ca.cert.pem 可以看到证书的内容。到这里，根私钥和根证书制作完毕。

#### 二. 创建中间证书对
1. mkdir ca/intermediate 创建这个目录用于存放中间证书。因此在 ca 目录下，我们有一个 intermediate 目录。

1.1. cd ca/intermediate

1.2. mkdir certs crl csr newcerts private 给 intermediate 目录创建3个子目录。看起来结构和 ca 目录一样。

1.3. chmod 700 private

1.4. 创建文件：touch index.txt

1.5. 创建文件：echo 1000 > serial

1.6. echo 1000 > crlnumber 这个文件用于存放证书吊销列表。

2. 创建中间商的私钥，以下操作在 ca 目录下：

2.1. 执行以下命令创建私钥 key（执行命令过程中会提示你输入密码）：
~~~
openssl genrsa -aes256 \
 
-out intermediate/private/intermediate.key.pem 4096 
~~~
2.2. chmod 400 intermediate/private/intermediate.key.pem  这个 intermediate.key.pem 就是中间商的私钥文件。

3. 制作中间商的证书。

3.1. 首先准备中间商需要的 openssl.cnf 文件。把前面那个 ca 底下的文件拷贝到 intermediate 目录下就可以了。

3.2. 执行以下命令制作证书 CSR：
~~~
openssl req -config intermediate/openssl.cnf -new -sha256 \
 
-key intermediate/private/intermediate.key.pem \
 
-out intermediate/csr/intermediate.csr.pem
~~~
执行上述命令的时候，会提示你输入中间商的私钥的密码。 

这里需要注意，在创建中间证书的时候，那个配置 .cnf 文件里面，A. 配置文件里面的 0.organizationName 的位置在命令需要你输入的时候，一定要输入和根证书相同的名字，大小写都不能错。B. commonName 一定要和根证书不同。执行命令后要求输入一些名字的时候，这个 commonName 一定要输入和根证书不同的名字。

3.3.用根证书为中间证书签名：
~~~
openssl ca -config openssl.cnf -extensions v3_intermediate_ca \
      -days 3650 -notext -md sha256 \
      -in intermediate/csr/intermediate.csr.pem \
      -out intermediate/certs/intermediate.cert.pem
~~~

3.4. chmod 444 intermediate/certs/intermediate.cert.pem

3.5. 校验这个中间证书：
~~~
openssl x509 -noout -text \
      -in intermediate/certs/intermediate.cert.pem
~~~
3.5. 当应用程序（例如Web浏览器）尝试验证由中间CA签名的证书时，它还必须对照根证书验证中间证书。 要完成信任链，请创建一个CA证书链以呈现给应用程序。

要创建CA证书链，请将中间证书和根证书连接在一起。 我们稍后将使用此文件来验证由中间CA签名的证书。

执行以下命令：
~~~
cat intermediate/certs/intermediate.cert.pem \
      certs/ca.cert.pem > intermediate/certs/ca-chain.cert.pem
	  chmod 444 intermediate/certs/ca-chain.cert.pem
~~~

3.6. 我们的证书链文件必须包含根证书，因为尚无客户端应用程序知道该证书。 更好的选择（尤其是在管理Intranet的情况下）是在需要连接的每个客户端上安装根证书。 在这种情况下，链文件仅需要包含您的中间证书。

#### 三. 创建网站使用的证书对
网站使用的证书对，采用中间证书来签发。制作过程和上面两个完全一样。

1. 创建 key - 也就是网站的私钥：
~~~
openssl genrsa -aes256 \
      -out intermediate/private/www.myhost.net.key.pem 2048
~~~
1.1. 注意，这里第一行那个 -aes256 是这个 key 的加密密码。网站第一次加载这个证书的时候，需要加载这个 key，需要输入密码。如果不想网站服务器加载这个key的时候输入密码，可以不要这个 -aes256 选项。

1.2. Delphi 的 Indy 提供的用于使用 OpenSSL 证书的控件 TIdServerIOHandlerSSLOpenSSL，有一个 OnGetPassword 事件，程序可以在这里输入上述 key 的密码用于程序使用这个 key。 

1.3. chmod 400 intermediate/private/mis.myhost.net.key.pem

 

2. 创建网站的证书：

2.1. 执行以下命令创建网站的 CSR：
~~~
cd /root/ca
# openssl req -config intermediate/openssl.cnf \
      -key intermediate/private/mis.myhost.net.key.pem \
      -new -sha256 -out intermediate/csr/www.myhost.net.csr.pem
~~~

 2.2. 在上述命令执行过程中，会提示你输入一堆信息，比如名字，邮箱等等。这里一定要注意，在输入 commonName 的地方，要输入网站的地址，一定不要和在制作中间证书时输入的 commonName 相同。相同则无法创建成功。

2.3. 为网站证书CSR使用中间证书签名，制作出最终要使用的证书：
~~~
openssl ca -config intermediate/openssl.cnf \
      -extensions server_cert -days 375 -notext -md sha256 \
      -in intermediate/csr/mis.myhost.net.csr.pem \
      -out intermediate/certs/mis.myhost.net.cert.pem
~~~
2.4. chmod 444 intermediate/certs/mis.myhost.net.cert.pem

2.5. 验证：
~~~
openssl x509 -noout -text \
      -in intermediate/certs/mis.myhost.net.cert.pem
~~~
到此，一个可以使用的证书制作完成。

这个证书可以用于网站的加密访问， 在我的博客文章里也讲了如何让 Delphi 的程序去使用它。 