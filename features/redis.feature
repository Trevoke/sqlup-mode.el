Feature: Upcasing redis as I type
  In order to type proper redis
  As a user
  I want to use sqlup-mode

  Background:
    Given I turn on redis-mode
    And I turn on sqlup-mode
    And the buffer is empty

  Scenario: Upcase a normal redis keyword after entering a space
    Given I insert "addslots"
    And I type " "
    Then I should see "ADDSLOTS "


