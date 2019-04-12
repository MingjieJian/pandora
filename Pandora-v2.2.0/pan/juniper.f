      subroutine JUNIPER
     $(MODE,INDEX,LODE,IU,IL,AR,IR,N,QNAME,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Constructs a call to a vector reader for arrays indexed
C     by "INDEX", which can be either: 'IJ',  'UL',  'NT'.
C     !DASH
      save
C     !DASH
      real*8 AR, W, Z, ZAUX
      integer IL, INDX, IR, IU, LODE, LZA, MODE, N
      character INDEX*2, QNAME*8
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MARCH, CILENTO, BASIL, CREAM, HALT, HI, BYE
C
      dimension W(*)
C
C               AR(N,*), IR(N,*), LZA(NZA), ZAUX(NZM,LZM), Z(N)
      dimension AR(N,*), IR(N,*), LZA(*),   ZAUX(*),       Z(*)
C
C
      call HI ('JUNIPER')
C     !BEG
      if((MODE.lt.1).or.(MODE.gt.2)) then
        write (MSSLIN(1),100) 'MODE', MODE
  100   format(A,' =',I12,', which is not 1 or 2.')
        call HALT    ('JUNIPER', 1)
      end if
      if((LODE.lt.1).or.(LODE.gt.2)) then
        write (MSSLIN(1),100) 'LODE', LODE
        call HALT    ('JUNIPER', 1)
      end if
C
      call MARCH     (IU, IL, INDEX, 'JUNIPER', INDX)
C
      if(MODE.eq.2) then
        call CILENTO (IR(1,INDX), N, QNAME)
      else
        if(LODE.eq.1) then
          call BASIL (AR(1,INDX), N, QNAME)
        else
          call CREAM (AR(1,INDX),    QNAME, IU, IL, LZA, ZAUX, Z, W)
        end if
      end if
C     !END
      call BYE ('JUNIPER')
C
      return
      end
