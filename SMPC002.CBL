000010*       SMPC002             LEVEL=1        DATE=26.03.18                    
000020******************************************************************          
000030*                                                                *          
000040*    TRANSACTION INFORMATION COPYBOOK                            *          
000050*    Luu tru thong tin giao dich                                 *          
000060*                                                                *          
000070*      DATE : 26.03.18                                           *          
000080*    UPDATE :                                                    *          
000090*                                                                *          
000100******************************************************************          
000110 01  (*)-PARA.                                                              
000120       03  (*)-IN-PARA.                                                     
000130         05  (*)-TRAN-NO               PIC  X(015).   *> 0001 So giao dich
000140         05  (*)-TRAN-DATE.                           *> 0016 Ngay giao dich
000150           07  (*)-TRAN-YY             PIC  9(004).   *> 0016   Nam
000160           07  (*)-TRAN-MM             PIC  9(002).   *> 0020   Thang
000170           07  (*)-TRAN-DD             PIC  9(002).   *> 0022   Ngay
000180         05  (*)-TRAN-TYPE             PIC  X(002).   *> 0024 Loai giao dich
000190         05  (*)-TRAN-AMOUNT           PIC S9(013)V99. *> 0026 So tien giao dich
000200         05  (*)-TRAN-CURRENCY         PIC  X(003).   *> 0041 Don vi tien te
000210         05  (*)-TRAN-ACCT-NO          PIC  X(016).   *> 0044 So tai khoan
000220         05  (*)-TRAN-BRANCH-CD        PIC  X(004).   *> 0060 Ma chi nhanh
000230         05  FILLER                    PIC  X(040).   *> 0064 Du phong
000240       03  (*)-OT-PARA.                                                     
000250         05  (*)-OT-TRAN-SEQ           PIC  9(010).   *> 0104 So thu tu giao dich
000260         05  (*)-OT-RESULT-CD          PIC  9(004).   *> 0114 Ma ket qua
000270         05  (*)-OT-MSG                PIC  X(100).   *> 0118 Thong bao
000280         05  (*)-OT-STATUS             PIC  9(001).   *> 0218 Trang thai
000290*                                                                           
