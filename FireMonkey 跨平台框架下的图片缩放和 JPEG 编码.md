# FireMonkey 跨平台框架下的图片缩放和 JPEG 编码

在 VCL 底下，把一个 Bitmap 变成 JPEG，是采用将 TBitmap 的内容赋值给 TJpegImage 的方式。

## 在 FireMonkey 底下该怎么做？
以下代码测试通过：
~~~
uses FMX.Surfaces;
 
procedure TForm1.Button1Click(Sender: TObject);
var
  Bmp: TBitmap;
  Src, Trg: TRectF;
  NewWidth, NewHeight: Integer;
  AStream: TFileStream;
  Surf: TBitmapSurface;
begin
  if OpenDialog1.Execute then
  begin
    Image1.Bitmap.LoadFromFile(OpenDialog1.FileName);
    Src := RectF(0, 0, Image1.Bitmap.Width, Image1.Bitmap.Height);
 
    //这里缩小一半
    NewWidth := Trunc(Image1.Bitmap.Width / 2);
    NewHeight := Trunc(Image1.Bitmap.Height / 2);
    Trg := RectF(0, 0, NewWidth, NewHeight);
 
    Bmp := TBitmap.Create;
    try
      Bmp.Width := NewWidth;
      Bmp.Height := NewHeight;
      Bmp.Canvas.BeginScene;
      try
        Bmp.Canvas.DrawBitmap(Image1.Bitmap, Src, Trg, 1)
      finally
        Bmp.Canvas.EndScene;
      end;
 
      Image2.Bitmap.Assign(Bmp);
 
      // 保存的文件名带 jpg ，自动压缩为 JPG
      Bmp.SaveToFile('D:\testAAA.jpg');
 
      //保存到 Stream 看看
      Surf := TBitmapSurface.Create;
      AStream := TFileStream.Create('D:\testAAABB.jpg', fmCreate);
      try
        Surf.Assign(Bmp);
        //这个方法，通过字符串 .jpg 指定它输出时编码为 jpg，测试通过。
        TBitmapCodecManager.SaveToStream(AStream, Surf, '.jpg', nil);
      finally
        AStream.Free;
        Surf.Free;
      end;
    finally
      Bmp.Free;
    end;
  end;
end
~~~

## 总结
1. **缩放**，在 VCL 底下采用 Canvas.StretchDraw 方法。这里是直接 DrawBitmap 就可以。

2. 输出为 **JPEG** 文件，直接使用 TBitmap.SaveToFile，它自动根据文件名的后缀决定如何编码。因此也可以输出为 PNG 文件。

3. 输出为 **Stream**，TBitmap.SaveToStream 没找到设置输出格式的参数。稍微麻烦一点，采用 TBitmapCodecManager.SaveToStream 方法，给出图片格式参数字符串。需要注意到是，jpg 前面必须加上小点。如果没加，也不会报错，但实际输出的是 Bitmpa 图片的内容而不是编码后的 JPEG 的内容。
