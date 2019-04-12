      subroutine WHAPS
     $(NSL,XNU,XNUK,WNU,WNUK,XNUC,WNUC)
C
C     Rudolf Loeser, 2004 May 25
C---- Prints frequencies and wavenumbers with more figures.
C     (This is version 2 of WHAPS.)
C     !DASH
      save
C     !DASH
      real*8 WNU, WNUC, WNUK, XNU, XNUC, XNUK
      integer I, NO, NSL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LINER, HI, BYE
C
C               XNU(NSL), WNU(NSL), XNUC(NSL), WNUC(NSL)
      dimension XNU(*),   WNU(*),   XNUC(*),   WNUC(*)
C
      call HI ('WHAPS')
C     !BEG
      call LINER (5, NO)
      write (NO,100)
  100 format(' ','Supplementary printout of model ion data,'/
     $       ' ','to show more significant figures.'//
     $       ' ',25X,'nu',21X,'nuc',14X,'wavenumber',20X,'wnuc')
      call LINER (1, NO)
      write (NO,101) (I,XNU(I),XNUC(I),WNU(I),WNUC(I),I=1,NSL)
  101 format(5(' ',I3,1P4E24.12/))
      call LINER (1, NO)
      write (NO,102) XNUK,WNUK
  102 format(' ',3X,1PE24.12,24X,E24.12)
C     !END
      call BYE ('WHAPS')
C
      return
      end
