module LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Shadow (offerHeroSacrifice) where

import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Message (Message)
import LordOfTheRings.Classes.HasQueue (pushAll)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Id

-- Each player may volunteer one of their own heroes to cancel the shadow.
offerHeroSacrifice :: ReverseQueue m => Source -> Target -> [PlayerId] -> [Message] -> m ()
offerHeroSacrifice _ _ [] effect = pushAll effect
offerHeroSacrifice source origin (pid : rest) effect = do
  heroes <- filterM discardable =<< select (HeroControlledBy $ PlayerWithId pid)
  if null heroes
    then offerHeroSacrifice source origin rest effect
    else chooseOneM pid do
      questionLabeled "lakeTown.cancelShadow"
      causedBy origin
      targets heroes $ discard source
      labeled "lakeTown.resolveShadow" $ offerHeroSacrifice source origin rest effect
 where
  discardable hid = do
    mods <- getModifiers (HeroTarget hid)
    immune <- immuneToSource source (HeroTarget hid)
    pure $ not immune && CannotLeavePlay `notElem` mods && CannotBeDiscardedByEncounterCards `notElem` mods
