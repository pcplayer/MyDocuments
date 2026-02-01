# CKEditor and Delphi WebBroker

## 下载和安装
  去官网下载： 
 http://ckeditor.com/ 

  我下载的是标准版。下载的打包文件，解压后，里面有一个 Samples 文件夹。底下有一个静态网页，演示如何在网页里加载这个 CKEditor。不过它的加载方式是，在网页里面有一个 DIV ID=editor，然后在单独写的一个 js 文件里面，将 editor 替换为编辑器。那个 js 比较复杂。使用那套方法，提交后，在后台读不到来自编辑器的内容。后台读不到编辑器这个元件

 在网页头部分，要引用 CKEditor 的 js: 
~~~
<script src="ckeditor/ckeditor.js"></script> 
~~~

### 网页
在网页中，需要出现编辑器的部分： 
~~~
      <form action="testWebBroker.exe/MyMemo"  method="post">
	<div class="adjoined-bottom">
		<div class="grid-container">
			<div class="grid-width-100">
				<div id="editor2">
					<textarea name="MyMemo" ></textarea>    <!-- 备注：这里是一个 textarea 元素 -->
					<script type="text/javascript"> CKEDITOR.replace("MyMemo"); </script>    <!--  这句话，将上述元素，替换为编辑器 -->
				</div>
			</div>
		</div>
		<div align="center">
			<input type="text" name="MyID" />
	                <input type="submit" value="submit"  />
		</div>
	</div>
        </form>
~~~

## WebBroker 后端代码
上述的 textarea 的 name="MyMemo"，在 WebBroker 的后端代码，读取 S := Request.ContentFields.Values['MyMemo']; 是可以读到用户在 CKEditor 编辑器里面输入的内容的。 

 上面的 form action= 里面的描述，是执行这个 cgi 的 action，这个 action 是 WebBroker 里面添加到一个 WebActionItem 用于响应用户点提交按钮的操作。在这个 WebActionItem 的 OnAction 事件代码里面，可以从 Request 读取到用户输入到 CKEditor 里面的内容。 
