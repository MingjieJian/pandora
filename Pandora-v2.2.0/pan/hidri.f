      subroutine HIDRI
     $(TE,CEQHH,N,QNAME,NHTSW,NH2CS)
C
C     Rudolf Loeser, 1982 May 28
C---- Sets up intermediates and switches for calculation of
C     Molecular Hydrogen number density.
C     (This is version 4 of HIDRI.)
C     !DASH
      save
C     !DASH
      real*8 CEQHH, TE
      integer KODE, LUEO, N, NH2CS, NHTSW
      logical HYDROGN
      character QNAME*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external FLUFF, BLUFF, GRUFF, MESHED, VECOUT, MASHED, HI, BYE
C
C               TE(N), CEQHH(N)
      dimension TE(*), CEQHH(*)
C
      call HI ('HIDRI')
C     !BEG
      KODE  = 0
      NH2CS = 0
C
      if(NHTSW.gt.0) then
        if(NHTSW.eq.1) then
          call FLUFF  (TE, CEQHH, N, KODE)
        else if(NHTSW.eq.2) then
          call BLUFF  (TE, CEQHH, N, KODE)
        else if(NHTSW.eq.3) then
          call GRUFF  (TE, CEQHH, N, KODE)
        end if
C
        if(KODE.gt.0) then
          call MESHED ('HIDRI', 3)
          write (LUEO,100) KODE,NHTSW
  100     format(' ','Trouble computing CEQHH, H2 abundance factor.'/
     $           ' ','Trouble at ',I3,'. depth.',10X,'NHTSW =',I2)
          call VECOUT (LUEO, TE, N, 'TE')
          call MASHED ('HIDRI')
C
          NH2CS = 0
        end if
C
        HYDROGN = (QNAME.eq.'H       ').or.(QNAME.eq.'HYDROGEN')
        if(HYDROGN.and.(KODE.eq.0)) then
C----     Set H2 number density recomputation switch since this is a
C         Hydrogen run and CEQHH was computed successfully
          NH2CS = 1
        end if
      end if
C     !END
      call BYE ('HIDRI')
C
      return
      end
