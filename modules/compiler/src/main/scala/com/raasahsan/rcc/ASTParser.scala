package com.raasahsan.rcc

import cats.syntax.all._
import cats.parse.Parser

object ASTParser {

  import AST._
  
  def translationUnit: Parser[TranslationUnit] = 
    externalDeclaration.rep.map(TranslationUnit(_))

  def externalDeclaration: Parser[ExternalDeclaration] = 
    functionDefinition.map(ExternalDeclaration.FunctionDefinition(_)) |
      declaration.map(ExternalDeclaration.Declaration(_))

  // TODO: can't compose Parser.With1 with Parser1
  def functionDefinition: Parser[FunctionDefinition] =
    (declarationSpecifiers.?.with1 ~ declarator ~ declarationList.? ~ compoundStatement).map {
      case (((specifiers, declarator), declarationList), statements) => FunctionDefinition(specifiers, declarator, declarationList, statements)
    }

  def declarationList: Parser[DeclarationList] =
    declaration.rep.map(DeclarationList(_))

  def declaration: Parser[Declaration] = ???

  def declarationSpecifiers: Parser[DeclarationSpecifiers] =
    declarationSpecifier.rep.map(DeclarationSpecifiers(_))

  def declarationSpecifier: Parser[DeclarationSpecifier] =
    storageClassSpecifier.map(DeclarationSpecifier.StorageClassSpecifier(_)) |
      typeSpecifier.map(DeclarationSpecifier.TypeSpecifier(_)) |
      typeQualifier.map(DeclarationSpecifier.TypeQualifier(_))

  // TODO: whitespace?
  def storageClassSpecifier: Parser[StorageClassSpecifier] =
    Parser.string("typedef").as(StorageClassSpecifier.Typedef) |
      Parser.string("extern").as(StorageClassSpecifier.Extern) |
      Parser.string("static").as(StorageClassSpecifier.Static) |
      Parser.string("auto").as(StorageClassSpecifier.Auto) |
      Parser.string("register").as(StorageClassSpecifier.Register)

  // TODO: finish with composite types
  def typeSpecifier: Parser[TypeSpecifier] = 
    Parser.string("void").as(TypeSpecifier.Void) |
      Parser.string("char").as(TypeSpecifier.Char) |
      Parser.string("short").as(TypeSpecifier.Short) |
      Parser.string("int").as(TypeSpecifier.Int) |
      Parser.string("long").as(TypeSpecifier.Long) |
      Parser.string("float").as(TypeSpecifier.Float) |
      Parser.string("double").as(TypeSpecifier.Double) |
      Parser.string("signed").as(TypeSpecifier.Signed) |
      Parser.string("unsigned").as(TypeSpecifier.Unsigned) 

  def typeQualifier: Parser[TypeQualifier] = 
    Parser.string("const").as(TypeQualifier.Const) |
      Parser.string("static").as(TypeQualifier.Static)

  def declarator: Parser[Declarator] =
    (pointer.?.with1 ~ directDeclarator).map { case (pointer, decl) => Declarator(pointer, decl) }

  def directDeclarator: Parser[DirectDeclarator] = 
    identifier.map(DirectDeclarator.Identifier(_)) |
      (Parser.string("(") *> declarator <* Parser.string(")")).map(DirectDeclarator.Declarator(_))


  def identifier: Parser[Identifier] = ???

  def pointer: Parser[Pointer] = ???

  def compoundStatement: Parser[CompoundStatement] = ???

}
