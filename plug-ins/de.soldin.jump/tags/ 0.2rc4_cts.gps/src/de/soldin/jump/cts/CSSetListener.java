/**
 * @(#)CSSetListener.java	29.06.2004
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.cts;


/**
 * This Interface defines the methods necessary for any
 * listener, which wants to be informed about changes in the cs
 * setting of a layer.
 * <p>
 * <code>CSSetListener</code>s can be registered with {@link CSSetting}
 * for that reason.
 * </p>
 * @see de.soldin.jump.cts.CSSetting
 */
public abstract interface CSSetListener {

	/**
	 * called when somebody tries to set to another cs
	 * 
	 * @param css the <code>CSSetting</code>
	 * @return if the trial was succesful
	 */
	public abstract boolean setTry(CSSetting css);
	
	/**
	 * called when the setting process was successful 
	 * 
	 * @param css the <code>CSSetting</code>
	 */
	public abstract void setDone(CSSetting css);

	/**
	 * called when the setting process failed at some point
	 * 
	 * @param css the <code>CSSetting</code>
	 */
	public abstract void setFailed(CSSetting css);
	
}
