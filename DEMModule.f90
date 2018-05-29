module DEMModule
    use MethodModule
    use ConstModule
    implicit none
contains
    function initF(filename, n) result(f)
        character(*) filename
        real(8), allocatable :: f(:,:), f_tmp(:,:)
        integer n, m, i, j, k
        
        f_tmp = readFile(filename, 4)
        n = size(f_tmp, 1)
        allocate(f(n,3))
        
        do i = 1, n 
            f(i,:) = f_tmp(i,2:)
        enddo
    end function initF
    
    function initD(points) result(d0)
        real(8), allocatable :: d0(:,:)
        real(8) points(:,:)
        integer n, i, j
        
        n = size(points, 1)
        allocate(d0(n,n))
        do i = 1,n
            do j = 1,n
                if(i > j) then
                    d0(i,j) = points(i,4) + points(j,4)
                else
                    d0(i,j) = 0.0d0
                endif
            enddo
        enddo
    end function initD
    
    function initX(points) result(x0)
        real(8), allocatable :: x0(:,:)
        real(8) points(:,:)
        integer n, i
        
        n = size(points, 1)
        allocate(x0(n,3))
        do i = 1, n
            x0(i,1) = points(i,2)
            x0(i,2) = points(i,3)
            x0(i,3) = 0.0d0 !回転を初期値で与える時はここで
        enddo
    end function initX
    
    subroutine makeF(F_all, F)
        real(8) F_all(:,:,:)
        real(8) F(:,:), f_tmp
        integer n, i, j, k
        
        n = size(F, 1)
        do i = 1, 3
            do j = 1, n 
                f_tmp = 0.0d0
                do k = 1, n
                    f_tmp = f_tmp + F_all(i,j,k)
                enddo
                F(j, i) = F(j, i) + f_tmp 
            enddo
        enddo
    end subroutine makeF
    
    subroutine makeD(x, d)
        real(8) x(:,:)
        real(8) d(:,:)
        integer n, i, j
        
        n = size(x,1)
        do i = 1, n
            do j = 1, n
                if(i > j) then
                    d(i,j) = sqrt((x(i,1) - x(j,1))**2 + (x(i,2) - x(j,2))**2)
                else
                    d(i,j) = 0.0d0
                endif
            enddo
        enddo
    end subroutine makeD
    
    subroutine checkSpringFlag(spring_flag, d, d0)
        integer spring_flag(:,:)
        real(8) d(:,:)
        real(8) d0(:,:)
        
        print *, "checkSpringFlag"
    end subroutine checkSpringFlag
    
    function calcf(x_i, x_j, u_i, u_j, r, e, spring) result(f)
        integer i, j
        real(8) x_i(:), x_j(:), u_i(:), u_j(:), r, e(:), spring(:)
        real(8) d_un, d_us, alpha
        real(8) fn, fs, f(3)
        
        alpha = calcAlpha(x_i, x_j)
        d_un = (x_i(1) - x_j(1))*cos(alpha) + (x_i(2) - x_j(2))*sin(alpha)
        d_us = -(x_i(1) - x_j(1))*sin(alpha) + (x_i(2) - x_j(2))*cos(alpha)
        
        e(1) = e(1) + spring(1) * d_un
        e(2) = e(2) + spring(2) * d_us

        fn = e(1) + spring(3) * d_un / delta_t
        fs = e(2) + spring(4) * d_us / delta_t
        
        f(1) = -fn*cos(alpha) + fs*sin(alpha)
        f(2) = -fn*sin(alpha) - fs*sin(alpha)
        f(3) = -r * fs
    end function calcf
    
    function calcAlpha(x1, x2) result(alpha)
        real(8) x1(:), x2(:)
        real(8) alpha
        alpha = atan2((x1(1) - x2(1)), (x1(2) - x2(2)))
    end function calcAlpha
    
    function makeSpring(filename, n) result(springs)
        character(*) filename
        real(8), allocatable :: springs(:,:,:), tmp(:,:)
        integer n, m, i, j, k
        
        tmp = readFile('spring.dat', 10)
        allocate(springs(n,n,8))
        springs = 0.0d0
        m = size(tmp,1)
        do i = 1, m
            springs(int(tmp(i,1)),int(tmp(i,2)),:) = tmp(i,3:)
        enddo
    end function makeSpring
    
    subroutine updateF_all(x, u, r, e, d, d0, spring_flag, springs, F_all)
        real(8) x(:,:)
        real(8) u(:,:)
        real(8) r(:)
        real(8) e(:,:,:)
        real(8) d(:,:)
        real(8) d0(:,:)
        integer spring_flag(:,:)
        real(8) springs(:,:,:)
        real(8) F_all(:,:,:)
        
        integer i, j, k, n
        real(8) f(3)
        
        F_all = 0.0d0
        call makeD(x, d)
        call checkSpringFlag(spring_flag, d, d0)
        
        n = size(x, 1)
        do i = 2, n
            do j = 1, (i-1)
                !if(spring_flag(i,j)) then 
                    f = calcf(x(i,:), x(j,:), u(i,:), u(j,:), r(i), e(i, j, :), springs(i,j,:))
                    do k = 1, 3
                        F_all(k, i, j) = f(k)
                        F_all(k, j, i) = -f(k)
                    enddo
                !endif
            enddo
        enddo
    end subroutine updateF_all
end module DEMModule