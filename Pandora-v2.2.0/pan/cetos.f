      subroutine CETOS
     $(X)
C
C     Rudolf Loeser, 1983 Nov 04
C---- Drives initialization of special Spectrum Save file.
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJBNL, JJBNU, JJFRR, JJMU, JJMUF, JJTE, JJVXN, JJVXS, JJZ
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(164),JJVXN)
C     !DASH
      external GLAUX, HI, BYE
C
      dimension X(*)
C
      call HI ('CETOS')
C     !BEG
      call GLAUX (X(JJZ), X(JJTE), X(JJVXS), X(JJFRR), X(JJVXN),
     $            X(JJMU), X(JJMUF), X(JJBNL), X(JJBNU))
C     !END
      call BYE ('CETOS')
C
      return
      end
