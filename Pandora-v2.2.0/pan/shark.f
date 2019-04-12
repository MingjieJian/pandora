      subroutine SHARK
     $(Z,N,NDT,FREQ,WAVE,OPAC,H,AA,BB,FINT,FF,GG,IMG,W,HK,HT)
C
C     Rudolf Loeser, 1987 Mar 24
C---- Computes flux-weighted-mean opacity and optical depth.
C     !DASH
      save
C     !DASH
      real*8 AA, BB, FF, FINT, FREQ, GG, H, HK, HT, OPAC, W, WAVE, Z
      integer IMG, IPEX, KODE, LUEO, N, NDT
      logical DMP, EDINT, EDTAU
      character LABEL*27, TITA*15, TITB*17
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external CARAWAY, ARRDIV, DANK, CHUKOR, MESHED, MASHED, HI, BYE
C
      dimension W(*)
C
C               GG(N,NDT), H(N,NDT), FREQ(NDT), OPAC(N,NDT), WAVE(NDT),
      dimension GG(*),     H(*),     FREQ(*),   OPAC(*),     WAVE(*),
C
C               BB(N), FINT(N), FF(N,NDT), AA(N), HK(N), HT(N), IMG(N),
     $          BB(*), FINT(*), FF(*),     AA(*), HK(*), HT(*), IMG(*),
C
C               Z(N)
     $          Z(*)
C
      data TITA  /'numerator of HK'/
      data TITB  /'denominator of HK'/
      data LABEL /'Flux-weighted optical depth'/
      data DMP   /.false./
C     !EJECT
C
      call HI ('SHARK')
C     !BEG
C---- Get integrands FF and GG
      call CHUKOR   (NDT, N, OPAC, H, FF, GG)
C---- Integrate FF to get AA
      call CARAWAY  (FF, NDT, N, FREQ, WAVE, AA, TITA, DMP)
C---- Integrate GG to get BB
      call CARAWAY  (GG, NDT, N, FREQ, WAVE, BB, TITB, DMP)
C---- Compute mean opacity
      call ARRDIV   (AA, BB, HK, N)
C---- Compute optical depth
      call DANK     (1, N, HK, Z, FINT, HT, LABEL, KODE, EDINT, EDTAU,
     $               IMG, W)
C
      if((IPEX.lt.0).or.(IPEX.eq.17)) then
        call MESHED ('SHARK', 2)
        write (LUEO,100) KODE,EDINT,EDTAU
  100   format(' ','KODE =',I12,', EDINT =',L3,', EDTAU =',L3)
        call MASHED ('SHARK')
      end if
C     !END
      call BYE ('SHARK')
C
      return
      end
