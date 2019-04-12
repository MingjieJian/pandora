      subroutine JOEY
     $(X,W,AR,IPER,K,N,I,OPAC,TAU,CNTRB,WTAB,IMG,NOPAC)
C
C     Rudolf Loeser, 1974 Mar 28
C---- Gets an edited set of TAU percentages, for contributors summaries.
C     (This is version 2 of JOEY.)
C     !DASH
      save
C     !DASH
      real*8 AR, CNTRB, OPAC, TAU, W, WTAB, X, ZERO
      integer I, IMG, IPER, J, K, KODE, M, N, NOPAC, jummy
      logical lummy1, lummy2
      character LABEL*100
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external DISMAL, MOVED, PEBBLES, HI, BYE
C
      dimension X(*), W(*)
C
C               AR(Nopac,N), CNTRB(Nopac), IPER(Nopac,Numkon), OPAC(N),
      dimension AR(NOPAC,*), CNTRB(*),     IPER(*),            OPAC(*),
C
C               TAU(N), IMG(N)
     $          TAU(*), IMG(*)
C
      call HI ('JOEY')
C     !BEG
C---- Convert opacities to TAUs
      do 102 J = 1,NOPAC
        KODE = 1
        do 100 M = 1,N
          OPAC(M) = AR(J,M)
          AR(J,M) = ZERO
          if(OPAC(M).le.ZERO) then
            KODE = 0
          end if
  100   continue
C
        if(KODE.eq.1) then
          write (LABEL,101) WTAB
  101     format(' Optical Depth',1PE20.12)
          call DISMAL (X, W, 1, N, OPAC, TAU, LABEL, jummy, lummy1,
     $                 lummy2, IMG)
          call MOVED  (TAU, 1, N, AR(J,1), NOPAC, N)
        end if
  102 continue
C
C---- Now get edited set of percentages
      call PEBBLES    (AR, IPER, CNTRB, K, N, I, NOPAC, 1)
C     !END
      call BYE ('JOEY')
C
      return
      end
