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
      order <- getPlayerOrder
      for_ order \p -> do
        idle <- selectCharacters (HeroControlledBy (PlayerWithId p) <> not_ CommittedHero) (AllyControlledBy (PlayerWithId p) <> not_ CommittedAlly)
        pushWhen (notNull idle) $ RaiseThreat p (length idle)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly)
      push
        $ if null allies
          then RaiseThreat pid 3
          else
            questionLabelWith "Return an exhausted ally to its owner's hand" (Just $ toTarget attrs) [] pid
              [targetLabel a [ReturnToHand (toTarget a)] | a <- allies]
      pure t
    _ -> PursuedByShadow <$> runMessage msg attrs
