      subroutine ANIAN
     $(X,W,N,RHEAB,VBMB,VM)
C
C     Rudolf Loeser, 1998 Feb 13
C---- (Re)computes VM, for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 RHEAB, VBMB, VM, W, X
      integer IFR, IHEND, IN, IS, JJFMV, JJHND, JJZ, MOX, N
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(225),JJFMV)
C     !DASH
      external  OSWY, TAMERS, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               RHEAB(N), VBMB(N), VM(N)
      dimension RHEAB(*), VBMB(*), VM(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IFR   ),(IN( 2),IHEND )
C
      call HI ('ANIAN')
C     !BEG
C     (Get W allotment)
      call OSWY   (IN,IS,MOX,'ANIAN')
C
      call TAMERS (N,X(JJZ),X(JJHND),RHEAB,VBMB,X(JJFMV),W(IFR),
     $             W(IHEND),VM)
C
C     (Give back W allotment)
      call WGIVE  (W,'ANIAN')
C     !END
      call BYE ('ANIAN')
C
      return
      end
