A new program to be created to assign/ unassign sales order to Handling
unit and transfer handling unit to sloc.

**Data Validation. Common for all the actions**

-   **HU Validation**

A new FM "ZSCM_HU_CHECK" to be created in AD2 to validate HU stock in
ewm.

Input:

Warehouse no -- Input from UI

Handling Units -- Input from UI

1.  Pass the input warehouse no and handling units to table /scwm/aqua
    table in field /scwm/aqua-WHN and /scwm/aqua-Handling Units
    respectively and check if all the handling units are found in table.
    If entry not found for any Handling unit then provide error and show
    that Handling unit in Error output.

-   **Sales order, Sales order line item, sloc validation.**

To validate Sales order create a new FM "ZSCM_SO_SLOC_VALIDATION" in RD2
with input sales order , Line item, warehouse and sloc. All fields will
be optional. In input either sales order and line item will come as
input or warehouse and sloc will come as input.

2.  To Validate sales order follow below logic.

    1.  Pass the input sales order to table VBAK- vbeln, if entry found
        go to next step ELSE provide error "Sales order does not exist.

    2.  Pass the sales order and line item to table VBAP in field vbeln
        and posnr respectively, if entry found give success message in
        final output ELSE provide error "Sales order line item does not
        exist".

3.  To validate Sloc follow below logic

    1.  Pass the input warehouse number to table T340D in field LGNUMWME
        and get LGNUM

    2.  Pass the LGNUM received in above step to table T320 in field
        LGNUM and get WERKS

    3.  Pass the WERKS (received in step 3.2) and Input SLOC in table
        T001L in field WERKS and LGORT respectively

    4.  If entry not found in table then give error "Entered SLOC is not
        valid" else provide success message.

-   **A function module Z_SOLAR_ACTIVITY is to be created to carry below
    activity.**

**[Part. 1. Assign sales order to Handling Unit.]{.underline}**

**Table Update header and Item table**

**Step1. To update header (ZPALTRFHDR) table**

1.  For each transaction generate one Transaction number

2.  Count the no of pallets received from input

3.  Update ZPALTRFHDR table as follow

31\. Transaction ID -- generated for transaction.

32\. No of pallets -- Count of input pallets

3.3 Created By -- System ID

3.4 Create on date -- System Date

3.5 Created on time -- System time

3.6. Activity -- What activity is selected from input screen

**Step2. To update item (ZPALTRFITM) table**

1.  To get the Lower level HU, Call FM "/SCWM/HU_SELECT_GEN" with below
    input.

    1.  IV_LGNUM -- Input warehouse no

    2.  IR_HUIDENT -- Input handling Units

> Output: ET_HUHDR
>
> ET_HUITM

3.  ET_HUHDR - GUID_HU = GUID of Handling unit

4.  ET_HUHDR -- HUIDENT = Handling unit no

5.  ET_HUHDR -- TOP = X then higher level HU

6.  ET_HUHDR -- BOTTOM = X then lower level HU

7.  ET_HUHDR -- HIGHER_GUID = This will be blank for Higher level HU and
    for lower level HU, GUID of Its Higher Level HU is updated.

8.  ET_HUITM- MATID = Material code against each lower level HU.

9.  ET_HUITM- CAT = Stock category against each lower level HU

```{=html}
<!-- -->
```
2.  Update **ZPALTRFITM** table as given below

2.1 Transaction ID -- generated for transaction.

2.2 Pallet Number -- HU number received in input.

2.3 Carton No -- Lower level HU received from step 1 for its higher
level hu.

**Step 3. Assign sales order to HU.**

1.  Check the count of input Handling Units and split it into multiples
    of 10. Eg. If the input Handling units are 94 then create 10 sets, 9
    sets of 10 HU each and remaining 4 HU in 10^th^ Set.

2.  We need to execute transaction set wise.

3.  Call FM "ZSCM_BIN_INTL_POST_GET_DET" as per below input.

Input.

1.  I_WAREHOUSE = Input warehouse

2.  I_ACTION = P1

3.  I_USER = System user id

4.  IT_INPUT- MATNR = ET_HUITM- MATID

5.  IT_HU- HU_ID = ET_HUHDR -- HUIDENT (only lower level of HU)

6.  IT_HU -STK_CAT = ET_HUITM- CAT

> Output.

7.  ET_OUTPUT

```{=html}
<!-- -->
```
4.  Once we get output from step 3 call FM "ZSCM_BIN_INTL_POSTING" with
    below input.

Input

4.1. IM_INPUT- WH_NUMBER = ET_OUTPUT -- LGNUM

4.2. IM_INPUT- SCREEN_ID = P1

4.3. IM_INPUT- STOCK_ID = ET_OUTPUT - STOCK_ID

4.4. IM_INPUT- PARENT_ID = ET_OUTPUT - PARENT_ID

4.5. IM_INPUT- DEST_STOCKDOCNO = Sales order received from input.

4.6. IM_INPUT- DEST_STOCKITMNO = sales order item no received from
input.

5.  Perform this activity in separate task so that screen should be free
    to perform another transaction.

6.  Once the Transaction is started update the status as "Running" and
    once the action is completed for all the sets then update the status
    of that transaction as "Completed" in ZPALTRFHDR table in STATUS
    field

**[Part. 2. Unassign sales order to Handling Unit.]{.underline}**

**Step1.Table Update header and Item table**

1.  Logic to update tables will remain same as mentioned in Part 1.

**Step2 .Unassign sales order to HU.**

1.  Check the count of input Handling Units and split it into multiples
    of 10. Eg. If the input Handling units are 94 then create 10 sets, 9
    sets of 10 HU each and remaining 4 HU in 10^th^ Set.

2.  We need to execute transaction set wise.

3.  Call FM "ZSCM_BIN_INTL_POST_GET_DET" as per below input.

Input.

1.  I_WAREHOUSE = Input warehouse

2.  I_ACTION = P2

3.  I_USER = System user id

4.  IT_INPUT- MATNR = ET_HUITM- MATID

5.  IT_HU- HU_ID = ET_HUHDR -- HUIDENT (only lower level of HU)

6.  IT_HU -STK_CAT = ET_HUITM- CAT

> Output.

7.  ET_OUTPUT

```{=html}
<!-- -->
```
4.  Once we get output from step 3 call FM "ZSCM_BIN_INTL_POSTING" with
    below input.

Input

4.1. IM_INPUT- WH_NUMBER = ET_OUTPUT -- LGNUM

4.2. IM_INPUT- SCREEN_ID = P2

4.3. IM_INPUT- STOCK_ID = ET_OUTPUT - STOCK_ID

4.4. IM_INPUT- PARENT_ID = ET_OUTPUT - PARENT_ID

5.  Perform this activity in separate task so that screen should be free
    to perform another transaction.

6.  Once the Transaction is started update the status as "Running" and
    once the action is completed for all the sets then update the status
    of that transaction as "Completed" in ZPALTRFHDR table in STATUS
    field

**[Part. 3. SFG to FG conversion]{.underline}**

**Step1.Table Update header and Item table**

1.  Logic to update tables will remain same as mentioned in Part 1.

> **[Step2 .SFG to FG conversion.]{.underline}**

1.  Check the count of input Handling Units and split it into multiples
    of 10. Eg. If the input Handling units are 94 then create 10 sets, 9
    sets of 10 HU each and remaining 4 HU in 10^th^ Set.

> 2\. We need to execute transaction set wise.

2.  Call FM "ZSCM_BIN_INTL_POST_GET_DET" as per below input.

Input.

a.  I_WAREHOUSE = Input warehouse

b.  I_ACTION = A2

c.  I_USER = System user id

d.  IT_INPUT- MATNR = ET_HUITM- MATID

e.  IT_HU- HU_ID = ET_HUHDR -- HUIDENT (only lower level of HU)

f.  IT_HU -STK_CAT = ET_HUITM- CAT

> Output.

g.  ET_OUTPUT

```{=html}
<!-- -->
```
3.  Once we get output from step 3 call FM "ZSCM_BIN_INTL_POSTING" with
    below input.

Input

6.1. IM_INPUT- WH_NUMBER = ET_OUTPUT -- LGNUM

6.2. IM_INPUT- SCREEN_ID = A2

6.3. IM_INPUT- STOCK_ID = ET_OUTPUT - STOCK_ID

6.4. IM_INPUT- PARENT_ID = ET_OUTPUT - PARENT_ID

5.  IM_INPUT- IM_LOCATION = Input SLOC no

```{=html}
<!-- -->
```
4.  Perform this activity in separate task so that screen should be free
    to perform another transaction.

5.  Once the Transaction is started update the status as "Running" and
    once the action is completed for all the sets then update the status
    of that transaction as "Completed" in ZPALTRFHDR table in STATUS
    field.
