      subroutine HERMAN
     $(LU,N,KWA,NKA,LWNT,NZ,KNT,INDEX,ALB,WAVE,Z,ALOMI,ALOMA,
     $ ZT,TAB,TABL,ARRL,WAVCA,ARRCA,TE,XNE,HND)
C
C     Rudolf Loeser, 1993 Sep 15
C---- Reads, massages and prints "averaged" line opacity data.
C     !DASH
      save
C     !DASH
      real*8 ALB, ALOMA, ALOMI, ARRCA, ARRL, DELTA, HND, TAB, TABL, TE,
     $       WAVCA, WAVE, XNE, Z, ZT
      integer I, INDEX, J, KNT, KWA, KZ, LU, LUEO, LUKA, LWNT, N, NKA,
     $        NZ
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(37),LUKA )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ARISOD, MOVED, SEAN, LACK, SHAWN, HI, BYE
C
C               INDEX(KNT), ALB(N), Z(N), ZT(NZ), WAVCA(KWA), TABL(NZ),
      dimension INDEX(*),   ALB(*), Z(*), ZT(*),  WAVCA(*),   TABL(*),
C
C               ARRL(KWA,KNTKU), ARRCA(KWA,N), HND(N), TAB(NZ), XNE(N),
     $          ARRL(*),         ARRCA(*),     HND(*), TAB(*),  XNE(*),
C
C               TE(N)
     $          TE(*)
C
      data DELTA /1.D-8/
C
      call HI ('HERMAN')
C     !BEG
C---- Read and examine the grid of Z-values (If KZ returns =1, then
C     the grid of Z-values and the Z-table of the run are the same.)
      read (LUKA) (ZT(I),I=1,NZ)
C
      call ARISOD    (ZT, NZ, Z, N, DELTA, KZ)
C
C---- Loop over all wavelengths
      do 100 J = 1,KWA
C----   Read raw data for this wavelength
        read (LUKA) (TAB(I),I=1,NZ)
        if(KZ.eq.1) then
C----     Use as is
          call MOVED (TAB, 1, NZ, ARRCA(J), KWA, N)
        else
C----     Interpolation required
          call SEAN  (J, KWA, N, Z, ARRCA, NZ, ZT, TAB, TABL)
        end if
  100 continue
C
C---- Return data file
      call LACK      (LUKA, LUEO)
C
C---- Print
      call SHAWN     (LU, ALOMI, ALOMA, ALB, TE, XNE, HND, WAVE, NKA,
     $                N, KWA, WAVCA, Z, ARRCA, LWNT)
C     !END
      call BYE ('HERMAN')
C
      return
      end
