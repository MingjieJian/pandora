      subroutine CHICKEN
     $(NO,N,NL,NLB,IQCSW,ALF,BATA,BATAR,BATAL)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Prints Alphas, and Stimulated Emission Factors.
C     (This is version 4 of CHICKEN.)
C     !DASH
      save
C     !DASH
      real*8 ALF, BATA, BATAL, BATAR
      integer IQCSW, N, NL, NLB, NO, jummy
      logical PRNTZ
C     !DASH
      external PRIAM, LINER, SCRIBE, OMAR, HI, BYE
C
C               ALF(MUL), BATA(N,MUL), BATAR(N,MUL), BATAL(N,NLB)
      dimension ALF(*),   BATA(*),     BATAR(*),     BATAL(*)
C
      data PRNTZ /.false./
C
      call HI ('CHICKEN')
C     !BEG
      if(NO.gt.0) then
C
        call PRIAM    (NO, 'ALPHA+BETA', 10)
C
        call LINER    (3, NO)
        write (NO,100)
  100   format(' ','Alpha   ( = 2*h*nu**3/c**2 )')
        call SCRIBE   (ALF  , 'UL', jummy, 1, 1, 1, NL, NO, jummy)
C
        call LINER    (3, NO)
        write (NO,101)
  101   format(' ','Stimulated emission factor, beta(T)  = ',
     $             'exp[ (-h*nu) / (k*T) ]')
C
        call LINER    (3, NO)
        write (NO,102)
  102   format(' ','Beta(TE)')
        call SCRIBE   (BATA,  'UL', jummy, 1, N, N, NL, NO, jummy)
C
        if(IQCSW.gt.0) then
          call LINER  (3, NO)
          write (NO,103)
  103     format(' ','Beta[TR(level 1)]')
          call SCRIBE (BATAR, 'UL', jummy, 1, N, N, NL, NO, jummy)
        end if
C
        call LINER    (3, NO)
        write (NO,102)
        call OMAR     (NO, N, NLB, BATAL, 'Level ', PRNTZ)
C
      end if
C     !END
      call BYE ('CHICKEN')
C
      return
      end
