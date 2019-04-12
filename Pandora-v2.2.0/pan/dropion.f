      subroutine DROPION
     $(W,LUM,CQUI,CQSI,GMI,RKI,RLI,CKI,CIJ,PIJ)
C
C     Rudolf Loeser, 1991 Aug 23
C---- Prints "minimal" RATES output.
C     !DASH
      save
C     !DASH
      real*8 CIJ, CKI, CQSI, CQUI, GMI, PIJ, RKI, RLI, W
      integer IMATR, IN, IRATE, IS, IVECT, LUM, MOX, N, NL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(121),IRATE)
C     !DASH
      external LINER, ARROUT, MOVED, VECOUT, NENEBUC, NUCLEUS, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               CQUI(N,NSL), CQSI(N,NSL), CIJ(N,NL,NL), PIJ(N,NL,NL),
      dimension CQUI(N,*),   CQSI(N,*),   CIJ(*),       PIJ(*),
C
C               RLI(N,NSL), CKI(N,NSL), RKI(N,NSL), GMI(N,NSL)
     $          RLI(N,*),   CKI(N,*),   RKI(N,*),   GMI(N,*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IVECT ),(IN( 2),IMATR )
C     !EJECT
C
      call HI ('DROPION')
C     !BEG
      if(LUM.gt.0) then
C       (Get, and allocate, W allotment)
        call NUCLEUS (IN,IS,MOX,'DROPION')
C
        call LINER   (5,LUM)
        write (LUM,100) IRATE
  100   format(' ','"Minimal" printout of computed RATES, at depth ',
     $             'index IRATE =',I4)
C
        call MOVED   (GMI(IRATE,1) ,N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'GM')
C
        call MOVED   (CQUI(IRATE,1),N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'QU')
C
        call MOVED   (CQSI(IRATE,1),N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'QS')
C
        call MOVED   (RKI(IRATE,1) ,N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'RK')
C
        call MOVED   (RLI(IRATE,1) ,N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'RL')
C
        call MOVED   (CKI(IRATE,1) ,N,NSL,W(IVECT),1,NSL)
        call VECOUT  (LUM,W(IVECT),NSL,'CK')
C
        call NENEBUC (CIJ,IRATE,W(IMATR))
        call ARROUT  (LUM,W(IMATR),NL,NL,'CIJ')
C
        call NENEBUC (PIJ,IRATE,W(IMATR))
        call ARROUT  (LUM,W(IMATR),NL,NL,'PIJ')
C
C       (Give back W allotment)
        call WGIVE   (W,'DROPION')
      end if
C     !END
      call BYE ('DROPION')
C
      return
      end
