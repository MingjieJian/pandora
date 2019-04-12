      subroutine SOMMER
     $(DUMP,N,NL,IU,IL,IM,Z,PSZ,PSAR,FSZ,FSAR)
C
C     Rudolf Loeser, 1987 Dec 11
C---- Detail printout for COMPLEX.
C     !DASH
      save
C     !DASH
      real*8 FSAR, FSZ, PSAR, PSZ, Z
      integer I, IL, IM, IU, LUEO, N, NL
      logical DMPI, DUMP, KILROY
      character TIT*13
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SKULL, LINER, DARROUT, MASHED, HI, BYE
C
C               Z(NL,NL,N), PSZ(N), PSAR(N), FSZ(N), FSAR(N)
      dimension Z(NL,NL,*), PSZ(*), PSAR(*), FSZ(*), FSAR(*)
C
      call HI ('SOMMER')
C     !BEG
      if(DUMP) then
        if(IM.eq.IU) then
          TIT = 'COMPLEX-Upper'
        else
          TIT = 'COMPLEX-Lower'
        end if
C
        KILROY = .true.
        do 101 I = 1,N
          call SKULL     (DUMP, KILROY, 'SOMMER', TIT, IU, IL, I, DMPI)
          if(DMPI) then
            call DARROUT (LUEO, Z(1,1,I), NL, NL, 'Matrix Z')
            call LINER   (2, LUEO)
            write (LUEO,100) PSZ(I),PSAR(I),FSZ(I),FSAR(I)
  100       format(' ','PSZ =',1PE24.16,', PSAR =',E24.16,', FSZ =',
     $                 E24.16,', FSAR =',E24.16)
          end if
  101   continue
        if(.not.KILROY) then
          call MASHED    ('SOMMER')
        end if
C
      end if
C     !END
      call BYE ('SOMMER')
C
      return
      end
