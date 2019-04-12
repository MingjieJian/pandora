      subroutine BULBUL
     $(WMN,WMX,WMNO,WMXO)
C
C     Rudolf Loeser, 2002 Mar 12
C---- Salvages obsolete RHO-weighting parameters.
C     (This is version 2 of BULBUL.)
C     !DASH
      save
C     !DASH
      real*8 ONE, TWO, WMN, WMNO, WMX, WMXO
      integer LUEO
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MESHED, MASHED, HI, BYE
C     !EJECT
C
      call HI ('BULBUL')
C     !BEG
      KILROY = .true.
C
      if(WMNO.ne.(-TWO)) then
        WMX = ONE-WMNO
C
        if(KILROY) then
          KILROY = .false.
          call MESHED ('BULBUL', 3)
        end if
        write (LUEO,100) WMX
  100   format(' ','The value of WRMX has been set =',1PE12.4,
     $             ', which = (1 - WMN).')
      end if
C
      if(WMXO.ne.(-TWO)) then
        WMN = ONE-WMXO
C
        if(KILROY) then
          KILROY = .false.
          call MESHED ('BULBUL', 3)
        end if
        write (LUEO,101) WMN
  101   format(' ','The value of WRMN has been set =',1PE12.4,
     $             ', which = (1 - WMX).')
      end if
C
      if(.not.KILROY) then
        call LINER  (1, LUEO)
        write (LUEO,102)
  102   format(' ','To avoid this automatic adjustment, replace the ',
     $             'old parameters with the new ones in the input ',
     $             'file.')
        call MASHED ('BULBUL')
      end if
C     !END
      call BYE ('BULBUL')
C
      return
      end
