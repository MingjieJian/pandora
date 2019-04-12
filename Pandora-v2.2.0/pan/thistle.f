      subroutine THISTLE
     $(DUMP,XLM,N,KUPT,FMULT,CAPPAR,CAPPA,SIGMA,OPAC,SCAT,SIGSTR,
     $ BHSNUM,BHSDEN,BHS,BHSNMS)
C
C     Rudolf Loeser, 2005 Oct 31
C---- Debug printout for background terms.
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSDEN, BHSNMS, BHSNUM, CAPPA, CAPPAR, FMULT, OPAC,
     $       SCAT, SIGMA, SIGSTR, XLM
      integer I, KUPT, LUEO, N
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, LINER, HI, BYE
C
C               SIGSTR(N), BHSNUM(N), BHSDEN(N), BHSNMS(N), CAPPAR(N),
      dimension SIGSTR(*), BHSNUM(*), BHSDEN(*), BHSNMS(*), CAPPAR(*),
C
C               CAPPA(N), SIGMA(N), OPAC(N), SCAT(N), BHS(N)
     $          CAPPA(*), SIGMA(*), OPAC(*), SCAT(*), BHS(*)
C
      call HI ('THISTLE')
C     !BEG
      if(DUMP) then
        call MESHED ('THISTLE', 2)
        write (LUEO,100) XLM,FMULT,KUPT
  100   format(' ','XLM =',1PE24.16,5X,'FMULT =',E12.5,5X,'KUPT =',I3//
     $         ' ',11X,'CAPPAR',7X,'CAPPA',7X,'SIGMA',8X,'OPAC',8X,
     $             'SCAT',6X,'SIGSTR',6X,'BHSNUM',6X,'BHSDEN',9X,'BHS',
     $             6X,'BHSNMS')
        call LINER  (1, LUEO)
        write (LUEO,101) (I,CAPPAR(I),CAPPA(I),SIGMA(I),OPAC(I),SCAT(I),
     $                      SIGSTR(I),BHSNUM(I),BHSDEN(I),BHS(I),
     $                      BHSNMS(I),I=1,N)
  101   format(5(' ',I5,1P10E12.4/))
      end if
C     !END
      call BYE ('THISTLE')
C
      return
      end
