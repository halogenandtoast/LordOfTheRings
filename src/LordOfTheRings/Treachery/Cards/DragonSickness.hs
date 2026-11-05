module LordOfTheRings.Treachery.Cards.DragonSickness (dragonSickness) where

import LordOfTheRings.Card (isTreasure)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Helpers.Treasure (treasuresAttachedTo)
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DragonSickness = DragonSickness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, from Mount Gundabad. Surge, on its card def.
dragonSickness :: TreacheryCard DragonSickness
dragonSickness = treachery DragonSickness Cards.dragonSickness

-- | Counts as a Condition attachment with the text: "Forced: After attached character
-- exhausts, raise your threat by 1 for each treasure attached to it."
instance HasAbilities DragonSickness where
  getAbilities (DragonSickness a) = [mkAbility a 1 $ forced $ ExhaustedWindow After host | AttachedTo host <- [a.placement]]

-- | The player controlling the character.
controllerOf :: HasGame m => Target -> m (Maybe PlayerId)
controllerOf = \case
  HeroTarget hid -> headMay <$> select (ControllerOfHero hid)
  AllyTarget aid -> placementController . (.placement) <$> getAlly aid
  _ -> pure Nothing

{- | When Revealed: Attach to a character with 1 or more treasures attached, then attach all
unguarded treasures in play to that character. The player who revealed it chooses; with no
such character it does nothing more. (A guarded treasure is not in play.)
-}
instance RunMessage DragonSickness where
  runMessage msg t@(DragonSickness attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      characters <- selectCharacters AnyHero AnyAlly
      hoarding <- filterM (fmap notNull . treasuresAttachedTo) characters
      chooseOrRunOneM revealer do
        questionLabeled "dragonSickness.character"
        causedBy attrs
        targets hoarding \c -> do
          attachTreachery attrs.treacheryId c
          others <- getsGame \g -> [toId x | x <- toList g.entities.attachments, isTreasure x, (toAttrs x).placement /= AttachedTo c]
          for_ others \aid -> moveAttachment aid c
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [h | AttachedTo h <- [attrs.placement]] \host -> do
        n <- length <$> treasuresAttachedTo host
        controllerOf host >>= traverse_ \pid -> when (n > 0) $ encounterRaisesThreat pid n
      pure t
    _ -> DragonSickness <$> runMessage msg attrs
