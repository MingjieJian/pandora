      subroutine AGALMA
     $(CALLER,N,NL,XND,XNK,VM,Z,SLVL,GXL,SION,GXI,LABEL,IS)
C
C     Rudolf Loeser, 1987 Oct 06
C---- Prints mass motion terms for upper levels, plus GXI.
C     (See also AGOUTA.)
C     !DASH
      save
C     !DASH
      real*8 GXI, GXL, SION, SLVL, VM, XND, XNK, Z
      integer I, IS, J, LUEO, MM, N, NL
      character CALLER*(*), LABEL*(*), SG*1, SIGNAL*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, ECKE, SHIM, PADGE, PODGE, MESHED, MASHED, HI, BYE
C
C               XND(N,NL), XNK(N), GXL(N,NL), GXI(N), SLVL(N,NL), Z(N),
      dimension XND(N,*),  XNK(*), GXL(N,*),  GXI(*), SLVL(N,*),  Z(*),
C
C               VM(N), SION(N), IS(N)
     $          VM(*), SION(*), IS(*)
C
      dimension SIGNAL(3)
C
      data SIGNAL /'_', ' ', '^'/
C
      call HI ('AGALMA')
C     !BEG
      call MESHED   (CALLER, 2)
C
      do 101 J = 2,NL
        call ECKE   (SLVL(1,J), N, IS)
        call PADGE  ( GXL(1,J), N, MM)
C
        call LINER  (2, LUEO)
        write (LUEO,102) 'GXL', LABEL, 'level', J
        call LINER  (1, LUEO)
        write (LUEO,103) 'ND', 'SLVL', 'GXL'
C
        call LINER  (1, LUEO)
        do 100 I = 1,MM
          SG = SIGNAL(IS(I)+2)
          write (LUEO,104) I,XND(I,J),Z(I),VM(I),SLVL(I,J),SG,GXL(I,J)
          call SHIM (I, 5, LUEO)
  100   continue
        call PODGE  (LUEO, MM, N, 104)
  101 continue
C     !EJECT
      call ECKE     (SION, N, IS)
      call PADGE    (GXI,  N, MM)
C
      call LINER    (3, LUEO)
      write (LUEO,102) 'GXI', LABEL, 'ion'
  102 format(' ',A,': mass motion term, ',A,', ',A,:,I3)
      call LINER    (1, LUEO)
      write (LUEO,103) 'NK', 'SION', 'GXI'
  103 format(' ',18X,A,19X,'Z',18X,'VM',16X,A,17X,A)
C
      call LINER    (1, LUEO)
      do 105 I = 1,MM
        SG = SIGNAL(IS(I)+2)
        write (LUEO,104) I,XNK(I),Z(I),VM(I),SION(I),SG,GXI(I)
  104   format(' ',I4,1P4E20.11,A1,E19.11)
        call SHIM   (I, 5, LUEO)
  105 continue
      call PODGE    (LUEO, MM, N, 104)
C
      call MASHED   (CALLER)
C     !END
      call BYE ('AGALMA')
C
      return
      end
