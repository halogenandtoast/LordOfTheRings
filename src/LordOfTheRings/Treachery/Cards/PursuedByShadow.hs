module LordOfTheRings.Treachery.Cards.PursuedByShadow (pursuedByShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PursuedByShadow = PursuedByShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pursuedByShadow :: TreacheryCard PursuedByShadow
pursuedByShadow = treachery PursuedByShadow Cards.pursuedByShadow

-- | When Revealed: Each player raises his threat by 1 for each character he controls that
-- is not currently committed to a quest.
-- Shadow: Defending player chooses and returns 1 exhausted ally he controls to its owner's
-- hand. If he controls no exhausted allies, raise his threat by 3.
instance RunMessage PursuedByShadow where
  runMessage msg t@(PursuedByShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        idle <- selectCharacters (HeroControlledBy (PlayerWithId p) <> not_ CommittedHero) (AllyControlledBy (PlayerWithId p) <> not_ CommittedAlly)
        when (notNull idle) $ encounterRaisesThreat p (length idle)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly)
      if null allies
        then encounterRaisesThreat pid 3
        else chooseOneM pid do
          questionLabeled "pursuedByShadow.returnAlly"
          causedBy attrs
          targets allies \a -> push $ ReturnToHand (toTarget a)
      pure t
    _ -> PursuedByShadow <$> runMessage msg attrs
