      subroutine FAKER
     $(N,NL,XNDW,XNDE,EDITED,XNK,XND,FRN,BDIW,BDI,BDIJ,NBAD,SET,NO)
C
C     Rudolf Loeser, 1980 Dec 03
C---- Prints final number densities and departure coefficients.
C     (This is version 2 of FAKER.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIJ, BDIW, FRN, SET, XND, XNDE, XNDW, XNK
      integer N, NL, NO, jummy
      logical EDITED, NBAD, PRNTZ
C     !DASH
      external PRIAM, ABJECT, LINER, PRIVET, SCRIBE, MEETOO, DPRIVE,
     $         OMAR, IBAR, HI, BYE
C
C               XNDE(N,NL), BDIW(N,NL), BDIJ(N,NL), XNK(N), XNDW(N,NL),
      dimension XNDE(*),    BDIW(*),    BDIJ(*),    XNK(*), XNDW(*),
C
C               FRN(N), BDI(N,NL), XND(N,NL), SET(N,MUL)
     $          FRN(*), BDI(*),    XND(*),    SET(*)
C
      data PRNTZ /.false./
C
      call HI ('FAKER')
C     !BEG
      if((NL.gt.1).and.(NO.gt.0)) then
        if(NBAD) then
          call PRIAM  (NO, 'ND PROBLEMS', 11)
          call LINER  (2, NO)
        else
          call ABJECT (NO)
        end if
C
        write (NO,100)
  100   format(' ','FRN: renormalization factor')
        call DPRIVE   (NO, FRN, N)
C
        call LINER    (3, NO)
        write (NO,101)
  101   format(' ','NK: ionized number densities - final, weighted, ',
     $             'renormalized')
        call PRIVET   (NO, XNK, N)
C     !EJECT
        if(EDITED) then
          call LINER  (3, NO)
          write (NO,102)
  102     format(' ','ND: number densities - weighted')
          call OMAR   (NO, N, NL, XNDW, 'Level ', PRNTZ)
C
          call LINER  (3, NO)
          write (NO,103)
  103     format(' ','ND: number densities - edited (weighted)')
          call IBAR   (NO, N, NL, XNDE, XNDW, 'Level ')
        else
          call LINER  (3, NO)
          write (NO,102)
          call OMAR   (NO, N, NL, XNDW, 'Level ', PRNTZ)
        end if
C
        call LINER    (3, NO)
        write (NO,104)
  104   format(' ','ND: number densities - final, renormalized')
        call OMAR     (NO, N, NL, XND, 'Level ', PRNTZ)
C
        call LINER    (3, NO)
        write (NO,105)
  105   format(' ','BDI: departure coefficients - weighted')
        call OMAR     (NO, N, NL, BDIW, 'Level ', PRNTZ)
C
        call LINER    (3, NO)
        write (NO,106)
  106   format(' ','BDI: departure coefficients - final, edited ',
     $             '(weighted)')
        call IBAR     (NO, N, NL, BDI, BDIW, 'Level ')
C
        call LINER    (3, NO)
        write (NO,107)
  107   format(' ','BDIJ: ratios of departure coefficients, from BDI')
        call MEETOO   (BDIJ, 1, N, NL, NO)
C
        call LINER    (3, NO)
        write (NO,108)
  108   format(' ','SET - stimulated emission terms')
        call SCRIBE   (SET, 'UL', jummy, 1, N, N, NL, NO, jummy)
      end if
C     !END
      call BYE ('FAKER')
C
      return
      end
