      subroutine MATACO
     $(XLM,KUPT,KOPAC,NOPAC,N,CO,CB,BHS,BHSDEN,BHSNUM,S1,SR,
     $ T1,TR,BHS1,BHSR,CAPPAR,BHSNMS,F)
C
C     Rudolf Loeser, 1986 Jul 12
C---- Computes Absorption Source function.
C
C     When KUPT > 0, KUPT is the number of the contributor whose
C     components have been reserved.
C     !DASH
      save
C     !DASH
      real*8 BHS, BHS1, BHSDEN, BHSNMS, BHSNUM, BHSR, CAPPAR, CB, CO, F,
     $       S1, SR, T1, TR, XDENR, XDENT, XLM, XNUMR, XNUMT
      integer I, KOPAC, KUPT, N, NOPAC
      logical UPT
C     !DASH
      external BUMP, DIVIDE, HI, BYE
C
C               CB(Nopac,N), BHSDEN(N), BHSR(N), BHS1(N), TR(N), T1(N),
      dimension CB(NOPAC,*), BHSDEN(*), BHSR(*), BHS1(*), TR(*), T1(*),
C
C               S1(N), SR(N), BHSNUM(N), BHSNMS(N), BHS(N), CAPPAR(N),
     $          S1(*), SR(*), BHSNUM(*), BHSNMS(*), BHS(*), CAPPAR(*),
C
C               KOPAC(Nopac), CO(Nopac,N)
     $          KOPAC(*),     CO(NOPAC,*)
C     !EJECT
C
      call HI ('MATACO')
C     !BEG
      UPT = KUPT.gt.0
C---- Loop over all depths
      do 100 I = 1,N
C----   Get numerator and denominator, without reserved contributions
C       (needed for separate absorption source functions, below)
        XDENR = CAPPAR(I)
        call BUMP   (KOPAC, CB(1,I), XNUMR)
C----   Get totals
        XDENT = XDENR
        XNUMT = XNUMR
        if(UPT) then
          CB(KUPT,I) = S1(I)+SR(I)
          XDENT = XDENT+CO(KUPT,I)
          XNUMT = XNUMT+CB(KUPT,I)
        end if
        BHSNMS(I) = XNUMT
C----   Form absorption source function, and save parts
        BHSDEN(I) = F*XDENT
        BHSNUM(I) = F*XNUMT
        call DIVIDE (BHSNUM(I), BHSDEN(I), BHS(I))
C----   Set up separated absorption source functions
        call DIVIDE (S1(I), T1(I), BHS1(I))
        call DIVIDE ((XNUMR+SR(I)), (XDENR+TR(I)), BHSR(I))
  100 continue
C     !END
      call BYE ('MATACO')
C
      return
      end
