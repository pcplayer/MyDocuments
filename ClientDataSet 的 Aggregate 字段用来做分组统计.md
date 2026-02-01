# ClientDataSet 的 Aggregate 字段用来做分组统计

**要点**  
Aggregate fields are virtual, persistent fields. While they are similar to other virtual, persistent fields, such as calculated and lookup fields, there is one very important difference. Specifically, introducing one or more aggregate fields does not preclude the automatic, runtime creation of dynamic fields. By comparison, creating at least one other type of persistent field, such as a data field, lookup field, or calculated field, prevents the ClientDataSet from creating other TFields for that ClientDataSet at runtime. As a result, it is always safe to create aggregate fields at design-time, whether or not you intend to instantiate any other TField instances at design-time.



As mentioned earlier, adding an aggregate field requires a number of specific steps in order to configure it correctly. These are:

  
  - Add the aggregate field to a ClientDataSet. This can be done at design-time using the Fields Editor, or at runtime using the TAggregateField's constructor.
  - 
Set the aggregate field's Expression property to define the calculation that the aggregate will perform

  - Set the aggregate field's IndexName property to identify the index to base grouping level on

  - Set the aggregate field's GroupingLevel property to identify which records to perform the aggregation across

  - Set the aggregate field's Active property to True to activate it
  - 
Set the aggregate field's Visible property to True
  - 
Set the AggregatesActive property of the ClientDataSet to which the aggregate is associated to True

## Setting Aggregate Index and Grouping Level
An aggregate needs to know across which records it will perform the calculation. This is done using the IndexName and GroupingLevel properties of the aggregate. Actually, if you want to perform a calculation across all records in a ClientDataSet, you can leave IndexName blank, and GroupingLevel set to 0.

If you want the aggregate to perform its calculation across groups of records, you must have a persistent index whose initial fields define the group. For example, if you want to calculate the sum of the AmountPaid field separately for each customer, and a customer is identified by a field name CustNo, you must set IndexName to the name of a persistent index whose first field is CustNo. If you want to perform the calculation for each customer for each purchase date, and you have fields named CustNo and SaleDate, you must set IndexName to the name of a persistent index that has CustNo and SaleDate as its first two fields.

The persistent index whose name you assign to the IndexName property can have more fields than the number of fields you want to group on. This is where GroupingLevel comes in. You set GroupingLevel to the number of fields of the index that you want to treat as a group. For example, imagine that you set IndexName to an index based on the CustNo, SaleDate, and PurchaseType fields. If you set GroupingLevel to 0, the aggregate calculation will be performed across all records in the ClientDataSet. Setting GroupingLevel to 1 performs the calculation for each customer (since CustNo is the first field in the index). Setting GroupingLevel to 2 will perform the calculation for each customer for each sale date (since these are the first two fields in the index).

It is interesting to note that the TIndexDef class, the class used to define a persistent index, also has a GroupingLevel property. If you set this property for the index, the index will contain additional information about record grouping. So long as you are setting an aggregate's GroupingLevel to a value greater than 0, you can improve the performance of the aggregate by setting the persistent index's GroupingLevel to a value at least as high as the aggregate's GroupingLevel. Note, however, that a persistent index whose GroupingLevel property is set to a value greater than 0 takes a little longer to generate and update, since it must also produce the grouping information. This overhead is minimal, but should be considered if the speed of index generation and maintenance is a concern.

## Making the Aggregate Field Available
The aggregate field is almost ready. In order for it to work, you must set the aggregate field's Active property and its Visible property to True. In addition, you must set the ClientDataSet's AggregatesActive property to True. After doing this, the aggregate will be automatically calculated when the ClientDataSet is made active.

With aggregate fields, there is one more step, which is associating the aggregate with a data-aware control (if this is what you want to do). The following steps demonstrate how to activate the aggregate, as well as make it visible in the DBGrid.

  1. With the aggregate field selected in the Object Inspector, set its Active property to True and its Visible property to True.
  2. Next, select the ClientDataSet and set its AggregatesActive property to True and its Active property to True.
  3. Now, right-click the DBGrid and select Columns. This causes the Columns collection editor to be displayed.
  4. Click the Add All button on the Columns collection editor toolbar to add persistent columns for each dynamic field in the ClientDataSet. 
  5. Now click the Add New button on the Columns collection editor toolbar to add one more TColumn.
  6. With this new TColumn selected, set its FieldName property to CustomerTotal. In order to see this calculated field easily, drag the new column to a higher position in the Columns collection editor. For example, move this new column to the third position within the Columns collection editor.
  7. That's it. If you have followed all of these steps, your newly added aggregate field should be visible in the third column of your DBGrid, as shown in the following figure.

A couple of additional comments about active aggregates are in order here. First, the ClientDataSet's AggregatesActive property is one that you might find yourself turning on and off at runtime. Setting AggregatesActive to False is extremely useful when you must add, remove, or change a number of records at runtime. If you make changes to a ClientDataSet's data, and these changes affect the aggregate calculation, these changes will be much slower if AggregatesActive is True, since the aggregate calculations will be updated with each and every change. After making your changes, setting AggregatesActive to True will cause the aggregates to be recalculated.

Rather than turning all aggregates off or on, the Active property of individual aggregates can be manipulated at runtime. This can be useful if you have many aggregates, but only need one or two to be updated during changes to the ClientDataSet. Subsequently turning other aggregates back on will immediately trigger their recalculation. At runtime you can read the ClientDataSet's ActiveAggs TList property to see which aggregates are currently active for a given grouping level. 

**精要**
1. 增加的 Aggregate 字段，要设置它的 Active 为 true，还要设置它的 Visible 为 True;


2. 还要设置 ClientDataset1 的 AggregatesActive 为 True;


3. 要设置 Aggregate 字段的 Expression 为 Sum(ShuLiang) 这里 ShuLiang 是数量字段，是我要合计的字段；

4. 要设置 Aggregate 字段的 IndexName，这个是为 ClientDataSet  增加的 Index 定义。这里是指设计期 ClientDataSet1 的 IndexDefs 属性增加一个索引定义，这个索引定义里面的索引字段是用于分组统计的分组字段。比如按客户编号统计客户购买数量，则这个索引的字段就是客户编号字段。    
    4.1. ClientDataSet1 的 IndexName 属性，要填入上面定义的 Index 的名字（而不是字段的名字）；

    4.2. Aggregate 字段里面的 IndexName 属性，也填入上面定义的 Index 的名字；


5. 要设置 Aggregate 字段的 GroupLevel，如果只有一个分组字段，则这里应该是 1；如果这里是 0 则是对所有全部字段作为一个分组，也就是把所有记录全部统计进来。


6. DBGrid 里面，手动添加一个字段，设置这个字段的名字是这个要显示的 Aggregates 字段。
