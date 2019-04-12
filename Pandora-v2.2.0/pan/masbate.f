      subroutine MASBATE
     $(NO)
C
C     Rudolf Loeser, 1986 Jul 08
C---- Prints scratch I/O control/performance data.
C     (See detailed remarks in "VISAYAS".)
C     !DASH
      save
C     !DASH
      real*8 XNRW, ZERO, dummy
      integer LUMA, NO, NRA, jummy
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(19),LUMA )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C     !DASH
      external LINER, RASHOW, RAKNTS, MESHOW, MEKNTS, HI, BYE
C
      call HI ('MASBATE')
C     !BEG
      if(NO.gt.0) then
        call LINER    (1, NO)
        call MEKNTS   (NRA, jummy, jummy, dummy, dummy, dummy)
        if(NRA.gt.0) then
          call MESHOW (NO)
          call LINER  (1, NO)
        end if
        call RAKNTS   (FILDAT, XNRW, dummy, dummy, dummy)
        if(XNRW.gt.ZERO) then
          call RASHOW (FILDAT, LUMA, jummy, NO)
          call LINER  (1, NO)
        end if
      end if
C     !END
      call BYE ('MASBATE')
C
      return
      end
