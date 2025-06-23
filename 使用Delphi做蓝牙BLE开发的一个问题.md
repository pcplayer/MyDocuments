# 使用Delphi做蓝牙BLE开发的一个问题
## 概念
Delphi 的 BLE 开发，拖一个 TBlueToothLe 到界面上，使用这个控件，就可以进行 BLE 的开发，比如去连接一个蓝牙手环。

Delphi 自带的 Demo 里面，有一个 BLEScanner 程序，可以作为开发的起点。

## 问题
上述程序，如果在 Windows 底下执行，扫描出设备后，如果鼠标点击某个设备，可能会界面冻结，程序没有响应。看任务管理区，确实程序没有了响应，死机了。

## 原因分析
鼠标点击，是执行对选中设备的服务的扫描。然后，当发现该设备的服务后，触发了 TBluetoothLE 的 OnServicesDiscovered 事件。在该事件中，循环读取该设备的多个服务，然后对某一个服务，循环读取它的 Character 的名字。死机正是循环读取 Character 那里。

为什么会死机，我不知道。但发现一个解决办法，代码如下：
~~~
procedure TForm6.BluetoothLE1ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  ServiceIndex: Integer;
  Service: TBluetoothGattService;
  CharacteristicIndex: Integer;
  Characteristic: TBluetoothGattCharacteristic;
begin
  //以下代码如果不包到 TTask.Run 里面（原本的代码没有），在 WINDOWS 底下，执行到 for 
  //CharacteristicIndex := 0 to Service.Characteristics.Count 会界面冻结，而且单步跟踪也停止 
  //了，没有往下执行。
  TTask.Run(  
  procedure
  var
    ServiceIndex: Integer;
    CharacteristicIndex: Integer;
  begin
    if AServiceList.Count > 0 then
    begin
      for ServiceIndex := 0 to AServiceList.Count - 1 do
      begin
        Service := AServiceList[ServiceIndex];
 
        TThread.Synchronize(nil,
          procedure
          begin
            Listbox2.Items.Add((ServiceIndex + 1).ToString + ' - ' + Service.UUIDName + ' - ' + Service.UUID.ToString);
          end
        );
 
 
        //以下代码会导致死机，如果断点跟踪，直接就是停在 for 这一行，不会继续往下执行。
        for CharacteristicIndex := 0 to Service.Characteristics.Count - 1 do
        begin
          Characteristic := Service.Characteristics[CharacteristicIndex];
 
          TThread.Synchronize(nil,
            procedure
            begin
              Listbox2.Items.Add('    - ' + Characteristic.UUIDName + ' - ' + Characteristic.UUID.ToString);
            end
          );
 
        end;
 
      end;
    end
    else
      TThread.Synchronize(nil,
        procedure
        begin
          Listbox2.Items.Add('- Access not allowed or no service available');
        end
      );
 
  end
  );
 
  //Listbox1.Enabled := True;
end;
~~~

## 代码解释
上述代码里面，TTask.Run 是我增加的。TThread.Synchronize 也是我增加的。去掉 TTask.Run 和 TThread.Synchronize 剩下的代码就是 Delphi 自带的 Demo 原本的代码。

首先，把原本的代码放进 TTask.Run 里面，就是把这些代码，放进了一个线程去执行。而不是让原本引发 OnServicesDiscovered 这个事件的线程去执行。可能原因：引发 OnServicesDiscovered 的这个线程，不能执行太多耗时的任务。

代码在线程里面执行，当需要往界面控件写数据时，比如 Listbox2.Items.Add 这样的代码，就需要做一个线程同步。因此加上  TThread.Synchronize。

我的开发环境

不同的环境下，可能现象不同。因此，这里要提一下我的开发环境：

Delphi 11 社区版；

Windows 11 家庭版；

编译运行的目标程序是 Win32 版本。

没有测试这个 Demo 在安卓底下是否会有上述问题。但我相信在安卓下，同样加上 TTask.Run 也会更好。

## Demo 程序的位置
这里提到的 Demo，安装了 Delphi 以后，如果是默认安装，这个 Demo 程序在：

C:\Users\Public\Documents\Embarcadero\Studio\22.0\Samples\Object Pascal\Multi-Device Samples\Device Sensors and Services\Bluetooth\BLEScanner

## 结论
使用 Delphi 开发 BLE 程序，比如做一个手环 APP，是没有问题的。但是需要留意，在 TBluetoothLE 这个控件的很多事件里面，最好不要执行太多的代码。如果有复杂的业务逻辑，最好放到单独的线程里面去执行，事件方法里面，仅仅是启动对应的线程。

对于 Delphi 来说，新增加的 TTask.Run 让我们把一大堆代码丢进线程执行，代码写法相对以前必须搞一个线程类，简单了很多。
