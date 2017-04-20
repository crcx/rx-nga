//
//  NgaBridge.h
//  RETRO12
//
//  Created by Charles Childers on 11/3/16.
//  Copyright © 2016 Charles Childers. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "nga.h"

@interface NgaBridge : NSObject
- (CELL)pop;
- (void)push:(CELL)value;
- (void)injectString:(NSString *)str into:(int)buffer;
- (NSString *)extractStringAt:(int)at;
- (int)getHeaderFor:(NSString *)name in:(CELL)dict;
- (int)getExecutionTokenFor:(NSString *)name in:(CELL)dict;
- (int)getClassHandlerFor:(NSString *)name in:(CELL)dict;
- (NSString *)executeFunctionAt:(int)cell;
- (void)update;
- (NSString *)evaluateToken:(NSString *)s;
- (NSArray *)stackValues;
- (NSString *)documentsDirectory;
- (void)closeAll;

@end
