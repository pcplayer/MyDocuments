# Delphi 的并行计算
下面的 Demo code 是在 FMX 里面对一张图片进行逐个像素的转换操作。图片是 RGB565格式，转换为 RGB888格式。

对图片像素的处理，传统写法是两层嵌套的循环，逐个像素处理：
~~~
procedure DoSomeConvert();
var
  i,j: Integer;
begin
  for i := 0 to ImageWidth -1 do
  begin
    for j := 0 to ImageHeight - 1 do
	begin
	  Handle-one-pixel;
	end;
  end;
end;
~~~
上面的代码，如果有100万个像素，就会顺序执行100万次。会很耗时。
现代 CPU 是多核的，如果把上述代码拆分为并行，让多个核同时执行，会显著缩短执行完成的时间。所谓的并行，在这里可以是多个核同时对不同的行逐像素进行运算。
并行通常就是采用多线程。Delphi 提供了一个类：**TParallel** 用于并行操作。

以下是对一张图片的一行里面每个像素的处理代码：
~~~
//这是计算一行的颜色值。其中 InputPtr 是 RGB565 的数据在内存里面的指针；ColorPtr 是 TBitmap 的 TBitmapData 的指针。 
//BTW: 以下代码有点问题：颜色值可能搞错，出来的结果，灰色正确，黄色变成了蓝色。图像清晰度没问题。
 
procedure TForm1.ConvertOneLine(const BmpWidth: Integer; InputPtr: PWord;
  ColorPtr: PByte);
var
  Col: Integer;
  Color: Word;
begin
  for Col := 1 to BmpWidth do
  begin
    Color := InputPtr^;
    Inc(InputPtr);
 
    ColorPtr^ := (( Color         and $1F) * $FF) div $1F;
    Inc(ColorPtr);
 
    ColorPtr^ := (((Color shr  5) and $3F) * $FF) div $3F;
    Inc(ColorPtr);
 
    ColorPtr^ := (((Color shr 11) and $1F) * $FF) div $1F;
    Inc(ColorPtr);
 
    ColorPtr^ := $FF;
    Inc(ColorPtr);;
  end;
end;
 
~~~

下面这个函数是对图片的每一行进行循环处理：
~~~
//以下代码，有串行计算的代码，也有并行计算的代码，都测试通过。
 
procedure TForm1.ConvertRGB565ToRGB8888(const BmpWidth, BmpHeight: Integer; RGB565DataPtr: PWord; BmpData: TBitmapData);
var
  InputPitch: Integer;
  Row, Col: Integer;
  Color: Word;
  ColorPtr: PByte;
  InputPtr: PWord; // Pointer to RGB565 data
begin
{------------------------------------------------------------------------
  把 RGB565 的数据转换到 FireMonkey 的 TBitmap 里面去。算法来自 Delphi 官方网站论坛里面的一个程序员对我的问题的回复。
------------------------------------------------------------------------}
  InputPitch:= BmpWidth*2;
 
 
  //将下面的串行循环，改为并行循环，在4核手机上测试通过。
  //大概比串行循环的时间少一半（并没有少 1/4）。
  TParallel.For(1, BmpHeight, procedure(Row: Integer)
  begin
    InputPtr:= PWord(PByte(RGB565DataPtr) + (Row-1)*InputPitch);
    ColorPtr:= PByte(BmpData.Data) + (BmpHeight-Row)*BmpData.Pitch;
 
    Self.ConvertOneLine(BmpWidth, InputPtr, ColorPtr);
  end
 
  );
 
  { 测试结果：以下串行计算代码执行正确。
  for Row := 1 to BmpHeight do
  begin
    InputPtr:= PWord(PByte(RGB565DataPtr) + (Row-1)*InputPitch);
    ColorPtr:= PByte(BmpData.Data) + (BmpHeight-Row)*BmpData.Pitch;
    Self.ConvertOneLine(BmpWidth, InputPtr, ColorPtr);
    //TParallel.&For
  end;
  }
end;
 
~~~
下面是主函数：
~~~
//以下代码是调用 ConvertRGB565ToRGB8888 的主程序：
 
procedure TForm1.DrawRGB656OntoBMP;
var
  //直接将 RGB656 的数据画到 BITMAP 上去试试看：
  ABitmap: TBitmap;
  BmpData: TBitmapData;
  BmpWidth: Integer;
  BmpHeight: Integer;
  RGB565DataPtr, InputPtr: PWord; // Pointer to RGB565 data
  AMemoryStream: TMemoryStream;
  Fn: string;
  T: TDateTime;
  InputPitch: Integer;
  Row, Col: Integer;
  Color: Word;
  ColorPtr: PByte;
begin
// 这段代码的测试结果：1. 在安卓下，直接出现浮点运算异常错误；
//2. 在 WINDOWS 底下，执行完成，但没有图像显示。并且转换过程耗时 200MS.跟踪 ScanlineToAlphaColor 函数内部，实际上是逐个点转换为 RGB888
  Fn := Memo1.Lines.Strings[0];
 
  AMemoryStream := TMemoryStream.Create;
  AMemoryStream.LoadFromFile(Fn);
 
  try
 
    //ABitmap := Image1.Bitmap;
 
    BmpWidth:= 1280;
    BmpHeight:= 720;
 
    Image1.Bitmap.SetSize(BmpWidth, BmpHeight);
    RGB565DataPtr := PWord(NativeInt(AMemoryStream.Memory) + 66);
    InputPitch:= BmpWidth*2; // Bytes by row
 
    T := Now;
    if Image1.Bitmap.Map(TMapAccess.Write, BmpData) then  //Map 方法取得 Bitmap 内部的数据结构，然后才能直接操作数据结构内部的指针。
    try
 
      Self.ConvertRGB565ToRGB8888(BmpWidth, BmpHeight, RGB565DataPtr, BmpData);
    finally
      Image1.Bitmap.Unmap(BmpData);
      Image1.UpdateEffects;
    end;
 
    Memo1.Lines.Add('time consuming = ' + IntToStr(MilliSecondsBetween(Now, T)));
  finally
    AMemoryStream.Free;
  end;
end;
 
~~~

## 测试结果：
对720P 的 RGB565 转码为 RGBA888，串行计算在 Find5 手机上耗时58MS，并行计算耗时 32MS。
