package io.github.poshjosh.ratelimiter.expression;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.*;

class SystemEnvironmentExpressionParserTest extends AbstractMappingStringExpressionParserTest {

    @Override ExpressionParser<Object, String> getExpressionParser() {
        return ExpressionParsers.ofSystemEnvironment();
    }

    @Override String getLHS() {
        return SystemEnvironmentExpressionParser.LHS;
    }


    private static class ValidParseArgumentProviderForSystemEnv extends ValidParseArgumentsProvider {
        public ValidParseArgumentProviderForSystemEnv() {
            super(SystemEnvironmentExpressionParser.LHS, "LANG", System::getenv);
        }
    }

    @ParameterizedTest
    @ArgumentsSource(ValidParseArgumentProviderForSystemEnv.class)
    void parse_shouldSucceed_givenValidExpression(String expressionStr, Expression<String> expected) {
        super.parse_shouldSucceed_givenValidExpression(expressionStr, expected);
    }

// The below test is used for clarity.
// The above test is confusing as the input and expected result is not immediately clear.
// TODO - Make the above tests and others like it clear
    @Test
    void parse_shouldSucceed_givenValidExpression() {
        final String expressionStr = "sys.environment[LANG] !<= C";
        final Expression<String> expected = Expressions.of("C", "!<=", "C");
        super.parse_shouldSucceed_givenValidExpression(expressionStr, expected);
    }
}