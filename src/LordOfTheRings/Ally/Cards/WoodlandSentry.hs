module LordOfTheRings.Ally.Cards.WoodlandSentry (woodlandSentry) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardDef (cdName), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Name (Name (..))

newtype WoodlandSentry = WoodlandSentry AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodlandSentry :: AllyCard WoodlandSentry
woodlandSentry = ally WoodlandSentry Cards.woodlandSentry

-- | Response: After you play Woodland Sentry from your hand, choose an enemy or location in
-- play. If there is a copy of that card in the victory display, discard the chosen card.
--
-- A copy is a card of the same title. Choosing one with no copy there does nothing, so
-- only those with one are offered.
instance HasAbilities WoodlandSentry where
  getAbilities (WoodlandSentry a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage WoodlandSentry where
  runMessage msg s@(WoodlandSentry a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      let title :: CardDef -> Text
          title def = let Name t _ = cdName def in t
          copied def = title def `elem` map (title . toCardDef) display
      enemies <- filterM (fmap (copied . toCardDef) . getEnemy) =<< select EnemyNotImmuneToPlayerCards
      locations <- filterM (fmap (copied . toCardDef) . getLocation) =<< select LocationNotImmuneToPlayerCards
      chooseOneM pid do
        questionLabeled "woodlandSentry.discard"
        causedBy a
        targets enemies (discard a)
        targets locations (discard a)
      pure s
    _ -> WoodlandSentry <$> runMessage msg a
