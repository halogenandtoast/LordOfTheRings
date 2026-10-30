module LordOfTheRings.Location.Cards.DarkHalls (darkHalls) where

import LordOfTheRings.Card (Card (PlayerCard), CardDef (cdName), toCardDef)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateWithShadow, reanimatedDeadInPlay)

newtype DarkHalls = DarkHalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkHalls :: LocationCard DarkHalls
darkHalls = location DarkHalls Cards.darkHalls

-- | While Dark Halls is in the staging area, each Reanimated Dead gets +1 attack and +1
-- defense.
instance HasModifiersFor DarkHalls where
  getModifiersFor (DarkHalls a) =
    when (a.placement == StagingArea) do
      dead <- reanimatedDeadInPlay
      for_ dead \e -> modified a e [AttackModifier 1, DefenseModifier 1]

-- | Shadow: If there is a copy of the defending character in your discard pile, reanimate
-- it and deal it 1 shadow card.
instance RunMessage DarkHalls where
  runMessage msg l@(DarkHalls a) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      discards <- (\p -> [c | PlayerCard c <- p.discard]) <$> getPlayer pid
      for_ defenders \d -> do
        defender <- characterCardDef d
        for_ (take 1 [c | c <- discards, title (toCardDef c) == title defender]) \c -> reanimateWithShadow c.id
      pure l
    _ -> DarkHalls <$> runMessage msg a
   where
    title def = case cdName def of Name t _ -> t
