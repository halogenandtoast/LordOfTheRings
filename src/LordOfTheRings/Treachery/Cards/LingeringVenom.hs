module LordOfTheRings.Treachery.Cards.LingeringVenom (lingeringVenom) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LingeringVenom = LingeringVenom TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lingeringVenom :: TreacheryCard LingeringVenom
lingeringVenom = treachery LingeringVenom Cards.lingeringVenom

-- | Surge.
-- When Revealed: Each player must choose: either exhaust each damaged character you
-- control, or deal 2 damage to the hero you control with the most damage on it.
-- Shadow: Exhaust each damaged character you control.
instance RunMessage LingeringVenom where
  runMessage msg t@(LingeringVenom attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        damaged <- damagedBy pid
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        damages <- for heroes \h -> (h,) <$> getDamage (toTarget h)
        let most = foldr (max . snd) 0 damages
        chooseOneM pid do
          questionLabeled "lingeringVenom.choose"
          causedBy attrs
          labeled "lingeringVenom.exhaust" $ for_ damaged exhaust
          labeled "lingeringVenom.damage" $ chooseOrRunOneM pid do
            questionLabeled "lingeringVenom.chooseHero"
            causedBy attrs
            targets [h | (h, n) <- damages, n == most] \h -> dealDamage attrs h 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      traverse_ exhaust =<< damagedBy pid
      pure t
    _ -> LingeringVenom <$> runMessage msg attrs
   where
    damagedBy pid = damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
