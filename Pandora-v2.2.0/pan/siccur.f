      subroutine SICCUR
     $(NO,AL,RKI,RLI,CKI,CQSI,SQS,SPKL,SLT,PIS,GMI,PIJ,CIJ,TIJ,DIJ)
C
C     Rudolf Loeser, 1998 Jun 23
C---- Analysis of effect of diffusion on (CIJ + PIJ).
C     !DASH
      save
C     !DASH
      real*8 AL, CIJ, CKI, CQSI, DIJ, GMI, PIJ, PIS, RKI, RLI, SLT,
     $       SPKL, SQS, TIJ, dummy
      integer I, IPDIJ, KDFA, N, NL, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (KZQ(164),IPDIJ)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(49),KDFA )
C     !DASH
      external GETPIJ, DIVIDE, LINER, NOTES, BIRGER, ABJECT, DASHER,
     $         HI, BYE
C
C               AL(NL), RKI(N,NSL), TIJ(N,NL,NL), PIJ(N,NL,NL), SQS(N),
      dimension AL(*),  RKI(*),     TIJ(*),       PIJ(*),       SQS(*),
C
C               CKI(N,NSL), PIS(N,NSL), GMI(N,NSL), RLI(N,NSL), SLT(N),
     $          CKI(*),     PIS(*),     GMI(*),     RLI(*),     SLT(*),
C
C               CIJ(N,NL,NL), SPKL(N), CQSI(N,NL), DIJ(N,NL,NL)
     $          CIJ(*),       SPKL(*), CQSI(*),    DIJ(*)
C     !EJECT
C
      call HI ('SICCUR')
C     !BEG
      if((NO.gt.0).and.(KDFA.eq.1)) then
C----   Compute "PIJ without diffusion"
        call GETPIJ   (   0   , AL, RKI, RLI, CKI, CQSI, SQS, SPKL,
     $                          SLT, PIS, GMI, dummy, TIJ)
C----   Form ratio
        do 100 I=1,(N*NL*NL)
          call DIVIDE ((CIJ(I)+PIJ(I)),(CIJ(I)+TIJ(I)),DIJ(I))
  100   continue
C----   Print
        call ABJECT   (NO)
        call DASHER   (NO)
        call LINER    (2, NO)
        write (NO,101)
  101   format(' ','Analysis of the effect of diffusion on ',
     $             '(CIJ + PIJ)    (printed when DIFFANA = on).'//
     $         ' ','TIJ = "PIJ" computed with GVL = 0'///
     $         ' ','DIJ = (CIJ + PIJ) / (CIJ + TIJ)'//
     $         ' ','Use IPDIJ = 0 for full printout, = 1 for ',
     $             'abbreviated printout.')
        if(IPDIJ.gt.0) then
          call BIRGER (NO, N, NL, DIJ)
        else
          call NOTES  (NO, N, NL, DIJ)
        end if
      end if
C     !END
      call BYE ('SICCUR')
C
      return
      end
