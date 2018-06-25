module DEMModule
    use MethodModule
    use ConstModule
    implicit none
contains
    function initF(filename) result(f)
        character(*) filename
        real(8), allocatable :: f(:,:), f_tmp(:,:)
        integer n, m, i, j, k
        
        f_tmp = readFile(filename, 7)
        n = size(f_tmp, 1)
        allocate(f(n,3))
        
        do i = 1, n 
            f(i,:) = f_tmp(i,2:4)
        enddo
    end function initF
    
    function initXContion(filename) result(x_condition)
        character(*) filename
        real(8), allocatable :: x_condition(:,:), x_condition_tmp(:,:)
        integer i, n
        
        x_condition_tmp = readFile(filename, 7)
        n = size(x_condition_tmp, 1)
        allocate(x_condition(n,3))
        
        do i = 1, n 
            x_condition(i,:) = x_condition_tmp(i,5:)
        enddo
    end function 
    
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
        integer i, j, n
        
        !call printArray2(d)
        n = size(d, 1)
        do i = 2, n
            do j = 1, (i-1)
                if(d(i,j) <= d0(i,j) * beta) then
                    spring_flag(i,j) = 1
                else
                    spring_flag(i,j) = 0
                endif
            enddo
        enddo
    end subroutine checkSpringFlag
    
    subroutine calcf(x_i, x_j, u_i, u_j, r, e, spring, f, i, j)
        integer i, j
        real(8) x_i(:), x_j(:), u_i(:), u_j(:), r, e(:,:,:), spring(:)
        real(8) d_un, d_us, alpha
        real(8) fn, fs, f(3)
        
        alpha = calcAlpha(x_i, x_j)
        d_un = (x_i(1) - x_j(1))*cos(alpha) + (x_i(2) - x_j(2))*sin(alpha)
        d_us = -(x_i(1) - x_j(1))*sin(alpha) + (x_i(2) - x_j(2))*cos(alpha)
        print *, d_un, d_us
        e(i,j,1) = e(i,j,1) + spring(1) * d_un
        e(i,j,2) = e(i,j,2) + spring(2) * d_us
        fn = e(i,j,1) + spring(3) * d_un / delta_t
        fs = e(i,j,2) + spring(4) * d_us / delta_t
        
        f(1) = -fn*cos(alpha) + fs*sin(alpha)
        f(2) = -fn*sin(alpha) - fs*sin(alpha)
        f(3) = -r * fs
    end subroutine calcf
    
    function calcAlpha(x1, x2) result(alpha)
        real(8) x1(:), x2(:)
        real(8) alpha, dx, dy
        dx = x1(1) - x2(1)
        dy = x1(2) - x2(2)
        if(dx == 0.0d0) then
            alpha = pi/2.0d0
        else
            alpha = atan(dy/dx)
        endif
        !alpha = atan2((x1(1) - x2(1)), (x1(2) - x2(2))) + atan2(0.0d0, 1.0d0)
    end function calcAlpha
    
    function makeSpring(filename, n) result(springs)
        character(*) filename
        real(8), allocatable :: springs(:,:,:), tmp(:,:)
        integer n, m, i, j, k
        integer index(2)
        
        tmp = readFile('spring.dat', 10)
        allocate(springs(n,n,8))
        springs = 0.0d0
        m = size(tmp,1)
        do i = 1, m
            index = makeIndex(tmp(i,1:2))
            springs(index(1), index(2), :) = tmp(i,3:)
        enddo
    end function makeSpring
    
    function makeIndex(tmp) result(index)
        real(8) tmp(:)
        integer index(2)
        if(tmp(1) < tmp(2)) then
            index(1) = int(tmp(2))
            index(2) = int(tmp(1))
        else 
            index(:) = int(tmp(:))
        end if
    end function makeIndex
    
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
        !call printArray2_int(spring_flag)
        n = size(x, 1)
        do i = 2, n
            do j = 1, (i-1)
                if(spring_flag(i,j) == 1) then 
                    print *, "calc"
                    call calcf(x(i,:), x(j,:), u(i,:), u(j,:), r(i), e, springs(i,j,:), f, i, j)
                    do k = 1, 3
                        F_all(k, i, j) = f(k)
                        F_all(k, j, i) = -f(k)
                    enddo
                else
                    do k = 1, 3
                        F_all(k, i, j) = 0.0d0
                        F_all(k, j, i) = 0.0d0
                    enddo
                endif
            enddo
        enddo
    end subroutine updateF_all
    
    subroutine solveEquation(F, m, x_new, x, x_old, x_condition)
        real(8) F(:,:)
        real(8) m(:,:)
        real(8) x_new(:,:), x(:,:), x_old(:,:)
        real(8) x_condition(:,:)
        
        integer n, i, j
        
        n = size(x,1)
        do i = 1, n
            x_new(i, 1) = (F(i,1) + (2*m(i,1)/(delta_t**2))*x(i,1) +  &
                          (m(i,1)/delta_t**2 - m(i,2)/(2*delta_t))*x_old(i,1)) &
                          / (m(i,1)/delta_t**2 + m(i,2)/(2*delta_t))
                          
            x_new(i, 2) = (F(i,2) + (2*m(i,1)/(delta_t**2))*x(i,2) +  &
                          (m(i,1)/delta_t**2 - m(i,3)/(2*delta_t))*x_old(i,2)) &
                          / (m(i,1)/delta_t**2 + m(i,3)/(2*delta_t))
                            
            x_new(i, 3) = (F(i,3) + (2*m(i,4)/(delta_t**2))*x(i,3) +  &
                          (m(i,4)/delta_t**2 - m(i,5)/(2*delta_t))*x_old(i,3)) &
                          / (m(i,4)/delta_t**2 + m(i,5)/(2*delta_t))
        enddo
        x_new = x_new*x_condition
    end subroutine solveEquation
end module DEMModule