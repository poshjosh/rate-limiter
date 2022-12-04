package com.wip;

public interface Rate2<T extends Rate2> extends Logical<T>{
    Amount getX();
    Amount getY();
}
