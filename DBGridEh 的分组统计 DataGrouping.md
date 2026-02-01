# DBGridEh 的分组统计 DataGrouping
有一个表，三个字段：

XuHao, ShuLiang, GuiGe

要根据 GuiGe 分类统计 ShuLiang.


通常用 DBGridEh 是这样的：
~~~
DBGridEh1.DataSource := DataSource1;
DataSource1.DataSet := ClientDataSet1;
~~~

如果要做分类统计的话，需要这样：
~~~
DGBridEh1.DataSource := DataSource2;
DataSource2.DataSet := MemTableEh1;
MemTableEh1.DataDriver := DataSetDriverEh1;
DataSetDriverEh1.ProviderDataSet := ClientDataSet1;
~~~

也就是说，如果要分组统计，必须让 DGBridEh 从 MemTableEh 获得数据来显示。

然后，设置好 DGBridEh.DataGrouping 这个属性就好了。全部可以在设计期搞定，无需代码。

以下描述，都是在设计期，在 Object Inspector 里面，针对 DbGridEh1 的 DataGrouping 属性，拉开后，设置里面的属性：

1. Active - True;
2. GroupPanelVisible - True; 注意如果只设置这两项，则运行时，它会显示一个 Panel 在 DBGrid 的顶上，并有英文提示，拖一个字段到这里，它会自动根据这个字段进行分组显示。
3. GroupLevels 点开，在弹出来的窗口里面，增加一条，类似增加一个字段一样的操作。
3.1. 在增加的这一条的属性里面，选择 ColumnName，下拉开，如果 DBGridEh1 已经在设计期加了静态字段，这里就会看到几个字段，挑选需要用来作为分组依据的字段。DBGridEh1 将会用这个字段来进行分组。
4. 分组统计：Footers 点开，弹出一个窗口，类似字段编辑器。增加一条。
    4.1. 选中增加的一条，属性里面选择 ColumnItems，会弹出窗口，窗口里面是前述的三个字段（必须是设计期已经为 DBGridEh1 增加了静态字段）。
    4.2. 选择 ShuLiang 字段，属性里面的 ValueType 下拉选择 gfvSumEh。就会在分组的 Footer 里面显示这一组的合计数字（小计）。
5. DefaultStateExpanded - True 表格会加载后自动展开。否则加载后，是收缩的，只显示组，不显示组里面的记录。

## 另外一个办法，是使用 ClientDataSet 的 Aggregates 字段来分组统计，然后用 DBGridEh 来显示
使用 ClientDataSet 的 Aggregates 来分组统计，也可以用 DBGrid 来显示。但使用 DBGrid 来显示，它会在每一行重复显示相同的分组统计数字，不好看。（有办法解决，请看本文结尾部分）

如果用 DBGridEh 来显示，则可以用以下代码：
~~~
with DBGridEh2 do
  begin
    for i := 0 to FieldCount -1 do
    begin
      if (Columns[i].FieldName = 'GuiGe') or (Columns[i].FieldName = 'HeJi') then
      Columns[i].HideDuplicates := True;
    end;
  end;
 
上述代码，对于规格字段和分组统计的合计字段，相同数字的，就不重复显示。这样更好看。
~~~
**备注**：上述代码是设置某个字段不要显示相同的数据。但是，如果连续的两个分组的“HeJi”字段的数字相同，则两个分组只显示一条，不太符合阅读要求。按照阅读要求，每个分组都要显示一条。

### 每个分组只显示一条合计
对于 DBGrid 或者 DBGridEh 来说，对于 ClientDataSet 的 Aggregates 字段，其实都可以在每个分组只显示一条，而不是重复显示。

1. 使用 Aggregtes 必须要先对 ClientDataSet1 本身，设置索引。这个索引的字段，就是用来分组的字段；因此，在 DBGrid 里面的显示，就是按照这个索引字段来排序的，也就是这个字段相同的记录会前后排到一起。因此，这时候，就可以显示为一组了；

2. 在上述的显示一个组里面，用于分组的字段本身是多条记录相同的，重复的，这里就可以多条记录只显示一条；

3. 在上述的显示的一个分组里面，用于 Aggregates 的字段（通常是分组的合计或者平均数）也是每条记录相同的，只需要显示一条；

4. 按照上述需求，实现代码如下：
~~~
procedure TDmMyData.CldSKUCostCOSTGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  //运费小计 COST 字段，一个分组只显示一次
  if gbFirst in CldSKUCost.GetGroupState(1) then
    Text := Sender.AsString
  else
    Text := '';
end;
~~~
**说明**：上述代码，是在该只需要一个分组只显示一条的字段的 OnGetText 里面写代码。