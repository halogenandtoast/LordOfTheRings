module LordOfTheRings.Location.Cards.EmbattledValley (embattledValley) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype EmbattledValley = EmbattledValley LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

embattledValley :: LocationCard EmbattledValley
embattledValley = location EmbattledValley Cards.embattledValley

instance HasModifiersFor EmbattledValley where
  getModifiersFor (EmbattledValley a) = do
    needed <- stagesWithoutProgress
    available <- exhaustible
    when (length available < needed) $ modifySelf a [CannotBeTraveledTo]

instance HasAbilities EmbattledValley where
  getAbilities (EmbattledValley a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage EmbattledValley where
  runMessage msg l@(EmbattledValley a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      n <- stagesWithoutProgress
      eachPlayer \pid -> do
        heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
        when (n > 0 && notNull heroes) $ dealDamageDividedAmong a pid heroes n
      pure l
    TravelTo lid | lid == a.id -> do
      n <- stagesWithoutProgress
      doStep n (WithTarget (toTarget a) msg)
      EmbattledValley <$> runMessage msg a
    DoStep n inner@(WithTarget (isTarget a -> True) (TravelTo lid)) | lid == a.id && n > 0 -> do
      available <- exhaustible
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "embattledValley.exhaust"
        causedBy a
        targets available \c -> do
          exhaust c
          doStep (n - 1) inner
      pure l
    _ -> EmbattledValley <$> runMessage msg a

exhaustible :: HasGame m => m [Target]
exhaustible = filterM (fmap (notElem CannotExhaust) . getModifiers) =<< selectCharacters ReadyHero ReadyAlly
