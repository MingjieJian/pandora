      subroutine DFAMAT
     $(AMT,CNOS,PART,PHEL,W,IW,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes A-matrix for DEEHEE (q.v.).
C     !DASH
      save
C     !DASH
      real*8 AMT, CNOS, FACT1, FACT2, FACT3, PART, PHEL, W, XREL, YREL,
     $       ZREL
      integer IW, KODE, LUEO
      logical DUMP
      character LABEL*50
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOTOR, HALT, DARROUT, HI, BYE
C
      dimension W(*), IW(*)
C
C               AMT(3,3), CNOS(5,5), PART(5)
      dimension AMT(3,*), CNOS(5,*), PART(*)
C
      data LABEL /'A-matrix for d-coefficients'/
C     !EJECT
C
      call HI ('DFAMAT')
C     !BEG
      XREL = PART(3)/PHEL
      YREL = PART(4)/PHEL
      ZREL = PART(5)/PHEL
      FACT1 = CNOS(3,1)+CNOS(3,2)
      FACT2 = CNOS(4,1)+CNOS(4,2)
      FACT3 = CNOS(5,1)+CNOS(5,2)
C
      AMT(1,1) =                                 FACT1
      AMT(1,2) = -(          CNOS(3,4)+CNOS(3,5)+FACT1*(     YREL+ZREL))
      AMT(1,3) = -(                    CNOS(3,5)+FACT1*(          ZREL))
C
      AMT(2,1) =                                 FACT2
      AMT(2,2) =  (CNOS(4,3)                    +FACT2*(XREL          ))
      AMT(2,3) = -(                    CNOS(4,5)+FACT2*(          ZREL))
C
      AMT(3,1) =                                 FACT3
      AMT(3,2) =  (CNOS(5,3)                    +FACT3*(XREL          ))
      AMT(3,3) =  (CNOS(5,3)+CNOS(5,4)          +FACT3*(XREL+YREL     ))
C
      if(DUMP) then
        call DARROUT (LUEO, AMT, 3, 3, 'A-matrix')
      end if
C
      call MOTOR     (AMT, 3, LABEL, W, IW, KODE)
      if(KODE.lt.1) then
        write (MSSLIN(1),100)
  100   format('No Plan-B available.')
        call HALT    ('DFAMAT', 1)
      end if
C
      if(DUMP) then
        call DARROUT (LUEO, AMT, 3, 3, 'Inverse of A-matrix')
      end if
C     !END
      call BYE ('DFAMAT')
C
      return
      end
