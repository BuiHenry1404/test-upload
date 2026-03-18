000010*       SMPC001             LEVEL=1        DATE=26.03.18                    
000020******************************************************************          
000030*                                                                *          
000040*    CUSTOMER INFORMATION COPYBOOK                               *          
000050*    Luu tru thong tin khach hang co ban                         *          
000060*                                                                *          
000070*      DATE : 26.03.18                                           *          
000080*    UPDATE :                                                    *          
000090*                                                                *          
000100******************************************************************          
000110 01  (*)-PARA.                                                              
000120       03  (*)-IN-PARA.                                                     
000130         05  (*)-CUST-NO               PIC  X(010).   *> 0001 Khach hang so
000140         05  (*)-CUST-NAME-X           PIC  X(060).   *> 0011 Ten khach hang (ASCII)
000150         05  (*)-CUST-NAME-N           PIC  N(030).   *> 0071 Ten khach hang (kanji)
000160         05  (*)-CUST-BIRTH.                          *> 0131 Ngay sinh
000170           07  (*)-CUST-BIRTH-YY       PIC  X(004).   *> 0131   Nam
000180           07  (*)-CUST-BIRTH-MM       PIC  X(002).   *> 0135   Thang
000190           07  (*)-CUST-BIRTH-DD       PIC  X(002).   *> 0137   Ngay
000200         05  (*)-CUST-GENDER           PIC  X(001).   *> 0139 Gioi tinh
000210         05  (*)-CUST-PHONE            PIC  X(015).   *> 0140 So dien thoai
000220         05  (*)-CUST-EMAIL            PIC  X(050).   *> 0155 Dia chi email
000230       03  (*)-OT-PARA.                                                     
000240         05  (*)-OT-RTN-CD             PIC  9(004).   *> 0205 Ma tra ve
000250         05  (*)-OT-MSG                PIC  X(080).   *> 0209 Thong bao loi
000260         05  (*)-OT-STATUS             PIC  9(001).   *> 0289 Trang thai xu ly
000270*                                                                           
