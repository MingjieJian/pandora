      subroutine KALMYK
     $(R1N,FRR,Z,N,EMU)
C
C     Rudolf Loeser, 1981 Sep 08
C---- Computes a set of values of EMU=cos(THETA),
C     for a ray intersecting the disk.
C     !DASH
      save
C     !DASH
      real*8 D2, DD, DR, EMU, FRR, R1N, RF, ROOT, Z, ZERO
      integer I, LUEO, N
      logical MESS
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, MESHED, MASHED, HI, BYE
C
C               Z(N), EMU(N)
      dimension Z(*), EMU(*)
C
      call HI ('KALMYK')
C     !BEG
      MESS = .true.
      RF   = (FRR*R1N)**2
C
      do 102 I = 1,N
        DR = R1N+(Z(N)-Z(I))
        D2 = DR**2
        DD = D2-RF
C
        if(DD.lt.ZERO) then
          if(MESS) then
            MESS = .false.
            call MESHED ('KALMYK', 3)
            write (LUEO,100) R1N,FRR,RF
  100       format(' ','Trouble in KALMYK, computed mu-values ',
     $                 'along rays for the spherical case.'/
     $             ' ','R1N=',1PE24.16,5X,'FRR=',E24.16,5X,
     $                 'RF=',E24.16)
          end if
          call LINER    (1, LUEO)
          write (LUEO,101) N,Z(N),I,Z(I), DR,D2,DD
  101     format(' ','N=',I5,5X,'Z(N)=',E24.16,5X,'I=',I5,5X,
     $               'Z(I)=',E24.16/
     $           ' ','DR=',E24.16,5X,'D2=',E24.16,5X,'DD=',E24.16)
C
          DD = ZERO
        end if
C
        ROOT   = sqrt(DD)
        EMU(I) = ROOT/DR
  102 continue
C
      if(.not.MESS) then
        call MASHED     ('KALMYK')
      end if
C     !END
      call BYE ('KALMYK')
C
      return
      end
