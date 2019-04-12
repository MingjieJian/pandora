      subroutine CLOD
     $(N,NL,GMI,XNDW,XND,BDIW,BDI,FOLD,IMG)
C
C     Rudolf Loeser, 2003 Apr 28
C---- Edits BD, in step with edited ND.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIW, FACT, FOLD, GMI, TERM, XND, XNDW, ZERO
      integer I, IMG, J, JNEDP, LUEO, MO, N, NL
      logical DUMP
      character BLANK*1, EQUAL*1, SIG*1
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
      equivalence (KZQ(193),JNEDP)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(44),EQUAL )
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external DIVIDE, MESHED, MASHED, LINER, HI, BYE
C
C               GMI(N,NSL), XNDW(N,NL), BDIW(N,NL), XND(N,NL), FOLD(N),
      dimension GMI(N,*),   XNDW(N,*),  BDIW(N,*),  XND(N,*),  FOLD(*),
C
C               BDI(N,NL), IMG(N)
     $          BDI(N,*),  IMG(*)
C     !EJECT
C
      call HI ('CLOD')
C     !BEG
      DUMP = (JNEDP.gt.0).and.(MO.gt.0)
      if(DUMP) then
        call MESHED        ('CLOD', 2)
        write (LUEO,100)
  100   format(' ','DUMP from updating B-values based on N-changes.')
      end if
C
      do 104 I = 1,N
        if(XND(I,1).gt.ZERO) then
          call DIVIDE      ((GMI(I,1)*BDIW(I,1)), XND(I,1), TERM)
C
          if(DUMP) then
            call LINER     (2, LUEO)
            write (LUEO,101) I,TERM,GMI(I,1),BDIW(I,1),XND(I,1)
  101       format(' ','Depth # =',I6,', term(1) =',1PE16.8/
     $             ' ',25X,'GM =',E16.8,', B =',E16.8,', N =',E16.8)
          end if
C
          do 103 J = 2,NL
            if(XND(I,J).ne.XNDW(I,J)) then
              call DIVIDE  (TERM, GMI(I,J), FACT)
              BDI(I,J) = FACT*XND(I,J)
C
              if(DUMP) then
                SIG = BLANK
                if(BDIW(I,J).eq.BDI(I,J)) then
                  SIG = EQUAL
                end if
                call LINER (1, LUEO)
                write (LUEO,102) J,GMI(I,J),XNDW(I,J),BDIW(I,J),
     $                           XND(I,J),BDI(I,J),SIG
  102           format(' ','l =',I5,10X,'GM =',1PE16.8,
     $                       ', Nold =',E25.17,', Bold =',E25.17/
     $                 ' ',40X,'Nnew =',E25.17,', Bnew =',E25.17,A1)
              end if
C
            end if
  103     continue
C
        end if
  104 continue
C
      if(DUMP) then
        call MASHED        ('CLOD')
      end if
C     !END
      call BYE ('CLOD')
C
      return
      end
