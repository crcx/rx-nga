//
//  NgaBridge.m
//  RETRO12
//
//  Created by Charles Childers on 11/3/16.
//  Copyright © 2016 Charles Childers. All rights reserved.
//

#import "NgaBridge.h"

@implementation NgaBridge
#define TIB 1471

FILE *Files[128];
CELL Dictionary, Heap, Compiler;
CELL notfound;

#pragma mark - Dictionary Interface

#define D_OFFSET_LINK  0
#define D_OFFSET_XT    1
#define D_OFFSET_CLASS 2
#define D_OFFSET_NAME  3

int d_link(CELL dt) {
    return dt + D_OFFSET_LINK;
}

int d_xt(CELL dt) {
    return dt + D_OFFSET_XT;
}

int d_class(CELL dt) {
    return dt + D_OFFSET_CLASS;
}

int d_name(CELL dt) {
    return dt + D_OFFSET_NAME;
}

- (int)getHeaderFor:(NSString *)name in:(CELL)dict {
    CELL dt = 0;
    CELL i = dict;
    NSString *dname;
    while (memory[i] != 0 && i != 0) {
        dname = [self extractStringAt:d_name(i)];
        if ([dname isEqualToString:name]) {
            dt = i;
            i = 0;
        } else {
            i = memory[i];
        }
    }
    return dt;
}

- (int)getExecutionTokenFor:(NSString *)name in:(CELL)dict {
    return memory[d_xt([self getHeaderFor:name in:dict])];
}

- (int)getClassHandlerFor:(NSString *)name in:(CELL)dict {
    return memory[d_class([self getHeaderFor:name in:dict])];
}

#pragma mark - Stack Interface

- (CELL)pop {
    sp--;
    return data[sp + 1];
}

- (void)push:(CELL)value {
    sp++;
    data[sp] = value;
    
}

#pragma mark - Inject / Extract Strings

- (void)injectString:(NSString *)str into:(int)buffer {
    int m = (int)[str length];
    int i = 0;
    while (m > 0) {
        memory[buffer + i] = (CELL)[str characterAtIndex:i];
        memory[buffer + i + 1] = 0;
        m--; i++;
    }
}

- (NSString *)extractStringAt:(int)at {
    NSMutableString *Output = [[NSMutableString alloc] initWithString:@""];
    CELL starting = at;
    while(memory[starting])
        [Output appendFormat:@"%c", (char)memory[starting++]];
    return Output;
}

#pragma mark - Interpreter

- (NSString *)executeFunctionAt:(int)cell {
    NSMutableString *Output = [[NSMutableString alloc] initWithString:@""];
    CELL opcode, subop;
    rp = 1;
    ip = cell;
    int nfp;
    int chr = 0;
    while (ip < IMAGE_SIZE) {
        if (ip == notfound) {
            [Output appendFormat:@"\nerr:notfound - %@\n", [self extractStringAt:TIB]];
        }
        opcode = memory[ip];
        if (ngaValidatePackedOpcodes(opcode) != 0) {
            ngaProcessPackedOpcodes(opcode);
        } else if (opcode >= 0 && opcode < 27) {
            ngaProcessOpcode(opcode);
        } else {
            switch (opcode) {
                case 1000:
                    [Output appendFormat:@"%c", data[sp]];
                    sp--;
                    break;
                case 1100:
                    subop = data[sp]; sp--;
                    switch (subop) {
                        case 1: [self openFile];
                            break;
                        case 2: // close
                            fclose(Files[data[sp]]);
                            Files[data[sp]] = 0;
                            sp--;
                            break;
                        case 3: // read
                            data[sp] = fgetc(Files[data[sp]]);
                            break;
                        case 4: // write
                            nfp = data[sp]; sp--;
                            chr = data[sp]; sp--;
                            fputc(chr, Files[nfp]);
                            break;
                        case 5: // position
                            [self push:[self getFilePosition]];
                            break;
                        case 6: // seek
                            [self setFilePosition];
                            break;
                        case 8: // delete
                            [self deleteFile];
                            break;
                        case 7: // length
                            [self push:[self getFileLength]];
                            break;
                        case 9: // count files
                            [self countFiles];
                            break;
                        case 10: // name for index
                            [self injectString:[self fileForIndex] into:TIB];
                            [self push:TIB];
                            break;
                        default:
                            [Output appendFormat:@"\n\nFATAL ERROR\nInvalid FILE operation at %d\n\n", ip];
                            ip = IMAGE_SIZE;
                            break;
                    }
                    break;
                default:
                    [Output appendFormat:@"\n\nFATAL ERROR\nInvalid instruction %d at %d\n\n", opcode, ip];
                    ip = IMAGE_SIZE;
                    break;
            }
        }
        ip++;
        if (rp == 0)
            ip = IMAGE_SIZE;
    }
    return Output;
}

- (void)update {
    Dictionary = memory[2];
    Heap = memory[3];
    Compiler = [self getExecutionTokenFor:@"Compiler" in:Dictionary];
    notfound = [self getExecutionTokenFor:@"err:notfound" in:Dictionary];
}

- (NSString *)evaluateToken:(NSString *)s {
    if ([s length] == 0)
        return @"";
    [self update];
    [self injectString:s into:TIB];
    [self push:TIB];
    return [self executeFunctionAt:[self getExecutionTokenFor:@"interpret" in:Dictionary]];
}

- (NSArray *)stackValues {
    NSMutableArray *values = [[NSMutableArray alloc] init];
    for (int i = 1; i <= sp; i++) {
        [values addObject:[NSNumber numberWithLong:data[i]]];
    }
    return values;
}

#pragma mark - File Access

- (NSString *)documentsDirectory {
    return [[[[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] lastObject] absoluteString] substringFromIndex:7];
}

- (void)openFile {
    CELL nfp, done;
    NSString *filename;
    nfp = 0;
    done = 0;
    while (nfp < 128 && done == 0) {
        if (Files[nfp] != 0)
            nfp++;
        else
            done = 1;
    }
    printf("NFP=%d", nfp);
    switch (data[sp]) {
        case 'R':
            sp--;
            filename = [NSString stringWithFormat:@"%@%@", [self documentsDirectory], [self extractStringAt:data[sp]]];
            NSLog(@"\nAttempt to open %@\n", filename);
            Files[nfp] = fopen([filename cStringUsingEncoding:NSUTF8StringEncoding], "r");
            data[sp] = nfp;
            break;
        case 'W':
            sp--;
            filename = [NSString stringWithFormat:@"%@%@", [self documentsDirectory], [self extractStringAt:data[sp]]];
            NSLog(@"\nAttempt to open %@\n", filename);
            Files[nfp] = fopen([filename cStringUsingEncoding:NSUTF8StringEncoding], "w");
            data[sp] = nfp;
            break;
        case 'A':
            sp--;
            filename = [NSString stringWithFormat:@"%@%@", [self documentsDirectory], [self extractStringAt:data[sp]]];
            NSLog(@"\nAttempt to open %@\n", filename);
            Files[nfp] = fopen([filename cStringUsingEncoding:NSUTF8StringEncoding], "a");
            data[sp] = nfp;
            break;
        default:
            break;
    }
}
- (CELL)getFilePosition {
    CELL slot = [self pop];
    return (CELL) ftell(Files[slot]);
}

- (CELL)setFilePosition {
    CELL slot, pos, r;
    slot = [self pop];
    pos = [self pop];
    r = fseek(Files[slot], pos, SEEK_SET);
    return r;
}

- (CELL)getFileLength {
    CELL slot, current, r, size;
    slot = [self pop];
    current = (CELL)ftell(Files[slot]);
    r = fseek(Files[slot], 0, SEEK_END);
    size = (CELL)ftell(Files[slot]);
    fseek(Files[slot], current, SEEK_SET);
    return (r == 0) ? size : 0;
}

- (void)reset {
    for (int i = 0; i < 128; i++)
        Files[i] = 0;
}

- (void)closeAll {
    for (int i = 0; i < 128; i++) {
        if (Files[i] != 0)
            fclose(Files[i]);
    }
    [self reset];
}

- (void)deleteFile {
    NSString *filename = [NSString stringWithFormat:@"%@%@",
                          [self documentsDirectory],
                          [self extractStringAt:[self pop]]];
    unlink([filename cStringUsingEncoding:NSUTF8StringEncoding]);
}

- (void)countFiles {
    CELL files = 0;
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    if ([paths count] > 0)
    {
        NSError *error = nil;
        NSFileManager *fileManager = [NSFileManager defaultManager];
        
        // Print out the path to verify we are in the right place
        NSString *directory = [paths objectAtIndex:0];
        NSLog(@"Directory: %@", directory);
        
        // For each file in the directory, create full path and delete the file
        for (NSString *file in [fileManager contentsOfDirectoryAtPath:directory error:&error])
        {
            NSString *filePath = [directory stringByAppendingPathComponent:file];
            NSLog(@"File : %@", filePath);
            files++;
        }
    }
    [self push:files];
}

- (NSString *)fileForIndex {
    CELL files = 0;
    CELL desired = [self pop];
    NSString *name = @"";
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    if ([paths count] > 0)
    {
        NSError *error = nil;
        NSFileManager *fileManager = [NSFileManager defaultManager];
        
        // Print out the path to verify we are in the right place
        NSString *directory = [paths objectAtIndex:0];
        NSLog(@"Directory: %@", directory);
        
        // For each file in the directory, create full path and delete the file
        for (NSString *file in [fileManager contentsOfDirectoryAtPath:directory error:&error])
        {
            NSString *filePath = [directory stringByAppendingPathComponent:file];
            NSLog(@"File : %@", filePath);
            files++;
            if (files == desired)
                name = file;
        }
    }
    return name;
}
@end
