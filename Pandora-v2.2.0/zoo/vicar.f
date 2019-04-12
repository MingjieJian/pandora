      subroutine VICAR
     $(LU,V,N,L)
C
C     Rudolf Loeser, 2002 Dec 04
C---- Basic vector printing routine.
C     L = 1 means: single precision,  4 significant figures;
C     L = 2 means: double precision, 16 significant figures.
C     !DASH
      save
C     !DASH
      real*8 V
      integer I, IE, IQ, IS, L, LN, LO, LU, N
      logical DOIT, MORE
      character BLANK*1, FORM*30, INDS*12, LINE*127, PINE*127
C     !DASH
      external  CAVIR, CRAVI, RIVAC
      intrinsic min
C
      dimension V(N), FORM(2), IQ(2)
C
      data BLANK /' '/
      data FORM  /'(A12,3X,1P5E11.3,2X,5E11.3)',
     $            '(A12,1P5E23.15)'/
      data IQ    /10, 5/
C
C     !BEG
C---- Check whether to print in full
      call CAVIR     (LU,V,N,L,DOIT)
      if(DOIT) then
        LINE = BLANK
        LO = 0
        LN = 0
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min((IE+IQ(L)),N)
          MORE = IE.lt.N
C
C----     Encode indices of elements on this line
          call CRAVI (IS,IE,INDS)
C----     Save previous line, and encode new one
          PINE = LINE
          write (LINE,FORM(L)) INDS,(V(I),I=IS,IE)
C----     Print line, or keep track of omitted identical lines
          call RIVAC (LU,MORE,LINE,PINE,LO,LN)
C
        if(MORE) goto 100
      end if
C     !END
C
      return
      end
