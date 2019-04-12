      subroutine AGOUTA
     $(CALLER,KAMB,N,NL,XND,XNK,ALPHA,Z,GHL,SLVL,GHI,SION,SHE,ADD,VL,VK,
     $ LABEL,IS)
C
C     Rudolf Loeser, 1987 Oct 06
C---- Prints ambipolar diffusion terms.
C     (See also AGALMA.)
C     !DASH
      save
C     !DASH
      real*8 ADD, ALPHA, GHI, GHL, SHE, SION, SLVL, VK, VL, XND, XNK, Z
      integer I, IS, J, KAMB, LUEO, MM, N, NL
      character CALLER*(*), LABEL*(*), SG*1, SIGNAL*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ALDO, SHIM, MESHED, MASHED, LAMBERT, PADGE, PODGE, ECKE,
     $         LINER, HI, BYE
C
C               XND(N,NL), XNK(N), SLVL(N,NL), GHI(N), SHE(N), SION(N),
      dimension XND(N,*),  XNK(*), SLVL(N,*),  GHI(*), SHE(*), SION(*),
C
C               ALPHA(NL), ADD(N), GHL(N,NL), IS(N), VL(N,NL), VK(N),
     $          ALPHA(*),  ADD(*), GHL(N,*),  IS(*), VL(N,*),  VK(*),
C
C               Z(N)
     $          Z(*)
C
      dimension SIGNAL(3)
C
      data SIGNAL /'_', ' ', '^'/
C     !EJECT
C
      call HI ('AGOUTA')
C     !BEG
      call MESHED   (CALLER, 2)
C
      call LAMBERT  (LUEO, N, KAMB, SHE, ADD)
C
      call LINER    (3, LUEO)
      call ALDO     (LUEO, ALPHA, NL)
C
      do 101 J = 1,NL
        call ECKE   (SLVL(1,J), N, IS)
        call PADGE  ( GHL(1,J), N, MM)
C
        call LINER  (2, LUEO)
        write (LUEO,102) 'GHL', LABEL, 'level', J
        call LINER  (1, LUEO)
        write (LUEO,103) 'ND', 'SLVL', 'GHL'
C
        call LINER  (1, LUEO)
        do 100 I = 1,MM
          SG = SIGNAL(IS(I)+2)
          write (LUEO,104) I,XND(I,J),Z(I),VL(I,J),SLVL(I,J),SG,GHL(I,J)
          call SHIM (I, 5, LUEO)
  100   continue
        call PODGE  (LUEO, MM, N, 84)
  101 continue
C
      call ECKE     (SION, N, IS)
      call PADGE    ( GHI, N, MM)
C
      call LINER    (2, LUEO)
      write (LUEO,102) 'GHI', LABEL, 'ion'
  102 format(' ',A,': ambipolar diffusion term, 'A,', ',A,:,I3)
      call LINER    (1, LUEO)
      write (LUEO,103) 'NK', 'SION', 'GHI'
  103 format(' ',18X,A,19X,'Z',17X,'"v"',16X,A,17X,A)
C
      call LINER    (1, LUEO)
      do 105 I = 1,MM
        SG = SIGNAL(IS(I)+2)
        write (LUEO,104) I,XNK(I),Z(I),VK(I),SION(I),SG,GHI(I)
  104   format(' ',I4,1P4E20.11,A1,E19.11)
        call SHIM   (I, 5, LUEO)
  105 continue
      call PODGE    (LUEO, MM, N, 84)
C
      call MASHED   (CALLER)
C     !END
      call BYE ('AGOUTA')
C
      return
      end
