module LordOfTheRings.Treachery.Cards.DragonSpell (dragonSpell) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DragonSpell = DragonSpell TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dragonSpell :: TreacheryCard DragonSpell
dragonSpell = treachery DragonSpell Cards.dragonSpell

-- | Counts as a Condition attachment with the text: "Attached hero gets -1 willpower for
-- each resource in its resource pool."
instance HasModifiersFor DragonSpell where
  getModifiersFor (DragonSpell a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      resources <- (.resources) <$> getHero hid
      when (resources > 0) $ modified a hid [WillpowerModifier (negate resources)]
    _ -> pure ()

{- | When Revealed: Attach to the hero with the most resources without Dragon-Spell
attached, the first player choosing among those tied.
"Forced: When attached hero spends resources, attached hero takes X damage. X is equal to
the number of resources spent."
-}
instance RunMessage DragonSpell where
  runMessage msg t@(DragonSpell attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      spelled <- getsGame \g -> [host | s <- toList g.entities.treacheries, toCardCode s == toCardCode Cards.dragonSpell, AttachedTo host <- [s.placement]]
      heroes <- filter ((`notElem` spelled) . toTarget) <$> (traverse getHero =<< select AnyHero)
      let most = foldr (max . (.resources)) 0 heroes
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "dragonSpell.attach"
        causedBy attrs
        targets [h.id | h <- heroes, h.resources == most] (attachTreachery attrs.treacheryId)
      pure t
    RemoveTokens (PlayerSource _) host Resource n | n > 0, AttachedTo host == attrs.placement -> do
      dealDamage attrs host n
      DragonSpell <$> runMessage msg attrs
    _ -> DragonSpell <$> runMessage msg attrs
