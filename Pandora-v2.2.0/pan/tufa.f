      subroutine TUFA
     $(ITAU,VAL,VLL,LAB)
C
C     Rudolf Loeser, 2004 Mar 04
C---- Fiddles for FOOT: edits VAL (if needed) and computes VLL.
C     !DASH
      save
C     !DASH
      real*8 SMALL, VAL, VLL, ZERO
      integer ITAU, LUEO
      character LAB*(*)
C     !COM
C---- ESPY        as of 2004 May 18
      logical     ESPION
      common      /ESPY/ ESPION
C     "Values range" constraint switch
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external SNAFU, MESHED, MASHED, HI, BYE
C
      data SMALL /-4.0D2/
C
      call HI ('TUFA')
C     !BEG
      if(VAL.gt.ZERO) then
        if(ESPION) then
          call SNAFU (VAL, VAL)
        end if
        VLL = log10(VAL)
      else
        call MESHED  ('TUFA', 3)
        write (LUEO,100) LAB,ITAU,SMALL
  100   format(' ',A,'(',I5,') = 0; log10 set =',F8.0)
        call MASHED  ('TUFA')
C
        VLL = SMALL
      end if
C     !END
      call BYE ('TUFA')
C
      return
      end
