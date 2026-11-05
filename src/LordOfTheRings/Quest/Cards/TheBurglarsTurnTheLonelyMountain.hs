module LordOfTheRings.Quest.Cards.TheBurglarsTurnTheLonelyMountain (theBurglarsTurnTheLonelyMountain) where

import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers

newtype TheBurglarsTurnTheLonelyMountain = TheBurglarsTurnTheLonelyMountain QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBurglarsTurnTheLonelyMountain :: QuestCard TheBurglarsTurnTheLonelyMountain
theBurglarsTurnTheLonelyMountain = quest TheBurglarsTurnTheLonelyMountain Cards.theBurglarsTurnTheLonelyMountain

instance HasAbilities TheBurglarsTurnTheLonelyMountain where
  getAbilities (TheBurglarsTurnTheLonelyMountain a) = [mkAbility a 1 $ forced $ QuestedSuccessfullyWindow After | a.flipped]

{- | Forced: After questing successfully, place 1 progress on Smaug the Golden. Then, the
first player makes a burgle attempt. If successful, the first player takes the top
treasure card from under The Lonely Mountain into his hand. Then, the players may advance
to stage 3: the first player decides, should this stage still be in play (Smaug the
Golden's sixth progress advances the players to stage 4A at once).

Forced: After the first player makes an unsuccessful burgle attempt, Smaug the Golden
attacks the first player.
-}
instance RunMessage TheBurglarsTurnTheLonelyMountain where
  runMessage msg q@(TheBurglarsTurnTheLonelyMountain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgressOnSmaugTheGolden attrs 1
      lead <- getLead
      mountain <- traverse getLocation =<< getTheLonelyMountain
      let top = [pc | Just l <- [mountain], PlayerCard pc <- take 1 (locationUnderneath $ toAttrs l)]
      makeBurgleAttempt attrs (for_ top \pc -> putInHand pc {pcOwner = Just lead}) (pure ())
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      lead <- getLead
      allowed <- notElem CannotBeDefeated <$> getModifiers attrs
      chooseOrRunOneM lead do
        questionLabeled "theBurglarsTurnTheLonelyMountain.advance"
        causedBy attrs
        when allowed $ labeled "theBurglarsTurnTheLonelyMountain.advanceToStage3" advanceQuest
        labeled "theBurglarsTurnTheLonelyMountain.stay" $ pure ()
      pure q
    (burgled -> Just (_, False)) | attrs.flipped -> do
      lead <- getLead
      traverse_ (immediateAttack lead) =<< getSmaugTheGolden
      pure q
    _ -> TheBurglarsTurnTheLonelyMountain <$> runMessage msg attrs
