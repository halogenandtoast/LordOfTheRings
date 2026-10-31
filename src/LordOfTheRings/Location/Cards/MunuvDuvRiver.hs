module LordOfTheRings.Location.Cards.MunuvDuvRiver (munuvDuvRiver) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MunuvDuvRiver = MunuvDuvRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

munuvDuvRiver :: LocationCard MunuvDuvRiver
munuvDuvRiver = location MunuvDuvRiver Cards.munuvDuvRiver

-- | While Munuv Dûv River is in the staging area, it gains: "Forced: After a player draws
-- any number of cards, he deals 1 shadow card to an enemy engaged with him."
instance HasAbilities MunuvDuvRiver where
  getAbilities (MunuvDuvRiver a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing | a.placement == StagingArea]

-- | Shadow: Raise your threat by 1 for each enemy engaged with you.
instance RunMessage MunuvDuvRiver where
  runMessage msg l@(MunuvDuvRiver a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (CardsDrawn pid _) <- ws] \pid -> do
        engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "munuvDuvRiver.shadow"
          targets engaged dealShadowCard
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      encounterRaisesThreat pid engaged
      pure l
    _ -> MunuvDuvRiver <$> runMessage msg a
