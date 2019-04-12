      subroutine HYPATIA
     $(FACTOR,Z,HNDO,OP5000,TAU5000,N)
C
C     Rudolf Loeser, 1994 Jan 14
C---- Checks whether the HND-adjustment factor is reasonable, and
C     aborts the run if not.
C     (This is version 2 of HYPATIA.)
C     !DASH
      save
C     !DASH
      real*8 FACTOR, FF, FL, HNAJL, HNDO, OP5000, TAU5000, Z
      integer LUEO, N
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
      equivalence (RZQ(144),HNAJL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  MESHED, VECOUT, ABORT, HI, BYE
      intrinsic abs
C
C               Z(N), HNDO(N), OP5000(N), TAU5000(N)
      dimension Z(*), HNDO(*), OP5000(*), TAU5000(*)
C
      call HI ('HYPATIA')
C     !BEG
      FF = log10(FACTOR)
      FL = log10(HNAJL)
C
      if(abs(FF).gt.abs(FL)) then
        call MESHED ('HYPATIA', 1)
        write (LUEO,100) FACTOR, HNAJL
  100   format(' ','Error in HYPATIA: HND-adjustment procedure for ',
     $             'obtaining TAU(5000) = 1 at Z = 0.'//
     $         ' ','FACTOR =',1PE12.4,' exceeds the specified input ',
     $             'limit HNAJL =',E12.4//
     $         ' ','(Remember options HSEDMP and NHADJ, as well as ',
     $             'input parameter TAUKIN.)')
C
        call VECOUT (LUEO, Z,       N, 'Z'      )
        call VECOUT (LUEO, HNDO,    N, 'HNDO'   )
        call VECOUT (LUEO, OP5000,  N, 'OP5000' )
        call VECOUT (LUEO, TAU5000, N, 'TAU5000')
        call ABORT
      end if
C     !END
      call BYE ('HYPATIA')
C
      return
      end
