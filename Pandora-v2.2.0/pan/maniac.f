      subroutine MANIAC
     $(X,IX,NLEV,MRJ,RRNUIJ,WRATIJ,MRX)
C
C     Rudolf Loeser, 1982 Jan 11.
C---- Computes MRX, the number of wavelength points available for the
C     rate integrations at each level, and returns the largest of these.
C     (This is version 2 of MANIAC.)
C     !DASH
      save
C     !DASH
      real*8 RRNUIJ, WAVEL, WAVEU, WRATIJ, X
      integer IBEG, IEND, IPEX, IQRK, IQRL, IX, J, KL, KU, LUEO, MRJ,
     $        MRX, NLEV, jummy
      logical OK
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
      external  SIERRA, MINK, RAYMOND, MESHED, MASHED, HI, BYE
      intrinsic max
C
      dimension X(*), IX(*)
C
C               MRZ = MRS+NSL+1
C
C               MRJ(NSL+1), RRNUIJ(MRZ), WRATIJ(MRZ)
      dimension MRJ(*),     RRNUIJ(*),   WRATIJ(*)
C     !EJECT
C
      call HI ('MANIAC')
C     !BEG
      MRX = 1
C
      do 101 J = 1,NLEV
C
        if(MRJ(J).gt.0) then
          call SIERRA    (X, IX, J, IQRK, IQRL)
          if((IQRK.eq.1).or.(IQRL.eq.1)) then
            call MINK    (J, MRJ, jummy, IBEG)
            IEND = IBEG+MRJ(J)
C
            WAVEU = WRATIJ(IBEG)
            call RAYMOND (WAVEU, 0, KU)
C
            WAVEL = WRATIJ(IEND)
            call RAYMOND (WAVEL, 0, KL)
C
            OK = (KU.ge.KL).and.(KU.ne.0)
C
            if(OK) then
              MRX = max(MRX,(KU  -KL  +1))
            else
              MRX = max(MRX,(IEND-IBEG+1))
            end if
C
          else
            OK = .true.
          end if
C
          if(((IPEX.lt.0).or.(IPEX.eq.12)).or.(.not.OK)) then
C----       Dump
            call MESHED  ('MANIAC', 2)
            write (LUEO,100) J,MRJ(J),MRX,
     $                       IBEG,RRNUIJ(IBEG),IEND,RRNUIJ(IEND),
     $                       WAVEU,KU,WAVEL,KL
  100       format(' ','J=',I4,2X,'MRJ=',I8,2X,'MRX=',I8,2X,
     $                 'RNU(',I8,')=',F10.6,2X,'RNU(',I8,')=',F10.6,/
     $             ' ','WAVEU=',1PE24.16,2X,'KU=',I8,2X,
     $                 'WAVEL=',  E24.16,2X,'KL=',I8)
            call MASHED  ('MANIAC')
          end if
        end if
C
  101 continue
C     !END
      call BYE ('MANIAC')
C
      return
      end
