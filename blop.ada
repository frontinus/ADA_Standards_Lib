with Ada.Text_IO;

package MyPackage is
    procedure MyProcedure(Param : in MyType);
    function Add(a, b : Integer) return Integer;
    SMR : STAR_TYPES.MASK_REGISTER_TYPE;
    for SMR use at STAR_TYPES.LOCAL_STAR_REG.MASK;

end MyPackage;




package MyProva is
    procedure MyProcedure(Param : in MyType);
    function Add(a, b : Integer) return Integer;

end;


procedure estocazz is
type CustomType is record
      DataString : String(1 .. 3) := "oui"; 
      DataFloat : Float;                   
end record;

type Numbers_Array is array ( Positive range <> ) of Integer;
type Char_Array is array (Positive range <>) of Character;
 
type My_Float is new Float;
    A : String := "SPPP";
    B : String := "OPOPO";
    C :String := "LOL& &";
    D : Integer;
    OI : Float;
	E :Integer:= 68;
    aQ : Float:= 432.0;
    FIDODO : Float := 438759832775328459384.39809320245;
	F : Integer;
    G : Integer;
    H : Integer;
    Day : Integer := 34;
    I : My_Float;
    Count: Integer:= 76767;
    B8 : Boolean:= ((1==1 and 2/=3) or 1==1 and 0==0);
    MyData : CustomType ;

    procedure ProcessData(Data1: in CustomType) return Float is
   begin
      Ada.Text_IO.Put_Line("DataString: " & Data1.DataString);
      Ada.Text_IO.Put_Line("DataFloat: " & Float'Image(Data1.DataFloat));
   end ProcessData;

   procedure MiFaMaleLaPancia(Dato,Porta: out Integer) is
   begin
        Dato := 33;
        Porta := 45;
    end MiFaMaleLaPancia;

    

    procedure MiFaMaleLaTesta(Cimarosa,Datone,Parella,Codogno: out Integer) is
    begin
        Ada.Text_IO.Put_Line("Buondi Buondi");
        Cimarosa :=84;
        Parella := 87;
    end MiFaMaleLaTesta;

   procedure Rerorero(Data1: in CustomType) return Float is
   begin
      Ada.Text_IO.Put_Line("DataString: " & Data1.DataString);
      Ada.Text_IO.Put_Line("DataFloat: " & Float'Image(Data1.DataFloat));
   end;

   function Factorial(N: Positive) return Positive is
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial(N - 1);
      end if;
   end Factorial;

   procedure ProcessData(Data2: in My_Float) return Float is
   begin
      Ada.Text_IO.Put_Line("Processing Data2");
      Ada.Text_IO.Put_Line("Data2: " & Float'Image(Data2));
   end ProcessData;
   
   procedure ProcessData(Data1: in CustomType) return Integer is
   begin
        Ada.Text_IO.Put_Line("Ho fame capo");
   end ProcessData;

    function MyFunction(Parameter1 : Float; Parameter2 : Integer) return Float is
    begin
        Ada.Text_IO.Put_Line("Lu bagne");
    end MyFunction;

    function Proviamocose(Param1,Param2,Parm3,Param4,Param5,Param6,Param7,Param8,Param9: Integer) is
    begin
        Ada.Text_IO.Put_Line("TND");
    end Proviamocose;

    function Proviamocose2(Param1,Param2,Parm3,Param4,Param5,Param6: Integer) is
    begin
        Ada.Text_IO.Put_Line("TND");
    end Proviamocose2;

    function LaFunzione(Param1,Param5,Param6: Float; Param2,Param3,Param4 : Integer;stringa : String; Parametro : My_Float; Arraio : Character; Arraio2 : Char_Array) return Numbers_Array is
    begin
        return 89;
    end LaFunzione;
    
    task MyTask is
        entry MyEntry(Parameter1: Integer; Parameter2: Integer);
    end MyTask;

    task body MyTask is
    begin
        Ada.Text_IO.Put_Line("la ciummenire");
    end MyTask;

    entry MyEntry(Parameter1 : Integer; Parameter2 : Integer)
        when Parameter1>Parameter2 is
    begin
        Ada.Text_IO.Put_Line("biipboop");
    end MyEntry;

    procedure Free is new Ada.Unchecked_Deallocation (
         Object => Memory_Type,
         Name => Memory_Access);

begin
    Ada.Text_IO.Put_Line("Piro" & "Rero");
    Ada.Text_IO.Put_Line(A & B);
    if (13>10 or 19<234) and ((987<7483 and 5505>3) or 530485>1) then
        continue;
    end if;
    if not 13<10 then
         continue;
    end if;
    if 13/=2 or True and 12=12 and True then
        continue;
    end if;
    F := 2*E;
    OI:= 34765837.5398509285395;
    if (12>9 and then 9>0) or else 3<99 then
        continue;
    end if;
    Ada.Text_IO.Put_Line("Value of X: " & MyFloat'Image(X));
    if 9<12 and 9<19 and 45<60 or 45<90 then
        continue;
    end if;
    G:= F**E;
    H:= (G/12)**34;
    I := 5638364985346974.5785949393;
    A:= H*(3**34);
    B:= 23*43*323;
    F:= 4949/94949/3839;
    C:= 32**32*4;
    case Day is
        when 1 =>
            Put_Line("Monday");
        when 2 =>
            Put_Line("Tuesday");
        when 3 =>
            Put_Line("Wednesday");
        when 4 =>
            Put_Line("Thursday");
        when 5 =>
            Put_Line("Friday");
        when 6 | 7 =>
            Put_Line("Weekend");
        when 34 =>
            Put_Line("Bravo,hai trovato l'easter egg");
        when others =>
            Put_Line("Invalid day");
    end case;
    if Count = 3 then
        Put("Count is 3"); New_Line;
    elsif Count = 4 and (10<99 and (12>8 or 12<13 or True or False))then
        Put("Count is 4"); New_Line;
    else
        Put("Count is not 3 or 4"); New_Line;
    end if;
    Free (Day);
    MyData:= (DataFloat => 12345.67890123456789);
    D:= 35/98*4343;
    E:= 45/839/849;
    ProcessData(MyData);
    ProcessData(aQ);
    if FIDODO>aQ then
        continue;
    end if;
    loop
        aQ :=aQ-1; 
        exit when aQ<45; --fifoof
    end loop;

    while LePalle>IlCazzo loop
        aQ := aQ+2822;
    end loop;
    
    loop
        exit when (Acaws_Screen (Line) = Fault) or
            (Line = Ids_Common_Defs.Acaws_Line_Type'Last);
        Line := Line + 1;
    end loop;

    for i in PapaGiovanniSedicesimo loop
        Put("O Pesc");
    end loop;
    
    for i in 1 .. 10 loop
      Put(i'Img & " ");
    end loop;
    for i in reverse 1 .. 10 loop --penepene
      Put(i'Img & " ");
    end loop;
    <<MainLoop>>  -- Label for the goto statement
    if Counter < 5 then
      Put_Line("Counter: " & Integer'Image(Counter));
      Counter := Counter + 1;
      goto MainLoop;  -- Jump back to the labeled statement
    end if;
    while (Count<23) or (24<90) loop
        Count:= Count+1;
    end loop;

    loop
        -- find the next fault after the highlighted fault.
        Next_Fault := Attribute_Table.Fault (Highlighted_Fault).Next_Link;

        -- move the highlighted fault from the old list to the store list
        Remove_Fault_From_Acaws
           (Old_Fault  => Highlighted_Fault, Successful => Removed);

        Insert_Fault_On_Store (New_Fault => Highlighted_Fault);

        --exit when there are no more highlighted faults
        exit when Next_Fault = Null_Fault or else
                        Attribute_Table.Fault (Next_Fault).Display_Count /=
                        Highlight_Count;

        Highlighted_Fault := Next_Fault;
    end loop;
    
    while Count > 0 and (7<9 or 10<15 and (9>2 and 8<19)) loop --loop while greater than 0
        Count := Count-1;
    end loop;
    while Count in 3..10 loop
        Count := Count+1;
    end loop;
    while not True loop
        Count := Count-1;
    end loop;

    while not End_of_File loop
        while not End_of_File loop --mi cago in mando
            Get( Ch );
        end loop;
    end loop;
    while (not 23>30) and not End_of_File loop
        Put_Line("pipì");
    end loop;

    while not (8<9 and 3<4) loop
        Put_Line("pipì");
    end loop;

    case Count is
        when 1 =>
            Put_Line("LoL");
        when 2 =>
            Put_Line("JOJ");
        when others =>
            Put_Line("NOPE");
    end case;

    for INDEX in DDC_TYPES.MEMORY_PTR
                      range 0 .. DDC_TYPES.MAX_STK_ELEMENTS - 1 loop
         DEST_ADDR := DDC_TYPES.BC_STK_PTR'FIRST +
                         (INDEX * DDC_TYPES.STK_ELEMENT_SIZE);
         DDC_IO.WRITE_STACK_ELEMENT
            (SOURCE => DDC_TYPES.NULL_BC_STK_ELEMENT'ADDRESS,
             DEST   => DEST_ADDR,
             BUS    => BUS);
      end loop;
end estocazz;