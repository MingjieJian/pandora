      subroutine KRAIT
     $(ION1,ION2,KODE)
C
C     Rudolf Loeser, 1981 Jan 30
C---- Converts the alphabetic description "ION1 <-> ION2"
C     into an equation code, for COULOMB.
C---- Equations are identified by the following interaction natrix:
C
C             E    H+   He+  He++ H    He
C
C     E       1    1    2    3    7    10
C     H+      1    1    0    0    6    12
C     He+     2    0    0    4    0    9
C     He++    3    0    4    0    0    0
C     H       7    6    0    0    5    11
C     He      10   12   9    0    11   8
C     !DASH
      save
C     !DASH
      integer K1, K2, KODE, LOOK1, LOOK2, LUEO, MATRIX
      character ION1*4, ION1S*4, ION2*4, ION2S*4, IONS*4
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LOOKUC, MESHED, ABORT, HI, BYE
C
      dimension MATRIX(6,6), IONS(6)
C
      data MATRIX /  1 ,     1 ,     2 ,     3 ,     7 ,    10 ,
     $               1 ,     1 ,     0 ,     0 ,     6 ,    12 ,
     $               2 ,     0 ,     0 ,     4 ,     0 ,     9 ,
     $               3 ,     0 ,     4 ,     0 ,     0 ,     0 ,
     $               7 ,     6 ,     0 ,     0 ,     5 ,    11 ,
     $              10 ,    12 ,     9 ,     0 ,    11 ,     8 /
C
      data IONS /'E   ', 'H+  ', 'HE+ ', 'HE++', 'H   ', 'HE  '/
C
      data ION1S,ION2S /'Z', 'Z'/
C     !EJECT
C
      call HI ('KRAIT')
C     !BEG
      KODE = -1
C
      if(ION1.ne.ION1S) then
        ION1S = ION1
        call LOOKUC (IONS, 6, ION1S, K1, LOOK1)
      end if
C
      if(ION2.ne.ION2S) then
        ION2S = ION2
        call LOOKUC (IONS, 6, ION2S, K2, LOOK2)
      end if
C
      if((LOOK1.eq.1).and.(LOOK2.eq.1)) then
        KODE = MATRIX(K1,K2)
      end if
C
      if(KODE.le.0) then
        call MESHED ('KRAIT', 1)
        write (LUEO,100) ION1,ION1S,ION2,ION2S,K1,K2,KODE,IONS
  100   format(' ','Trouble getting an equation number for Coulomb ',
     $             'cross-sections.'//
     $         ' ',4('[',A4,']',4X),3I8//
     $         ' ',6('[',A4,']',4X))
        call ABORT
      end if
C     !END
      call BYE ('KRAIT')
C
      return
      end
